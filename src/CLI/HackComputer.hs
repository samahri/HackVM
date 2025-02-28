{-# LANGUAGE OverloadedStrings #-}
module CLI.HackComputer (
    main
) where

import BasicPrelude hiding (putStrLn)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (evalStateT, execState, execStateT)
import System.IO (hSetBuffering, hSetEcho, stdin, hReady, BufferMode( NoBuffering ), withFile, IOMode(ReadMode), putStrLn)
import Data.ByteString as BS hiding (foldl)
import System.Exit(exitFailure)

import Nand2Tetris.Utils
import Nand2Tetris.HackComputer
import Nand2Tetris.Memory
import Nand2Tetris.Chips
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Assembler
import CLI.Assembler hiding (main)

main :: IO ()
main = do
    setupComputer

    (_, hackFileEither) <- getFileNames

    hackFile <- case hackFileEither of
        Right hackFile -> pure hackFile
        Left _ -> putStrLn "no file exists" >> exitFailure

    hackCode <- bytecodeToHackAssem <$> readBinaryContent hackFile
    
    initialComputerState <- loadMemory hackCode
    
    (memoryState, screenState, _, _, _, _) <- execStateT (hackComputer Zero >> runComputer hackComputer) initialComputerState
    
    -- for now, output is only reading from memory location memoryAddressToRead
    let memoryAddressToRead = pure Zero
    memOutput <- evalStateT (mainMemory memoryAddressToRead (pure Zero) Zero) (memoryState, screenState)
    print memOutput

setupComputer :: IO ()
setupComputer = hSetBuffering stdin NoBuffering >> hSetEcho stdin False

runComputer :: (Reset -> HackComputer) -> HackComputer
runComputer computer = do
    liftIO $ threadDelay 10
    computer Zero
    exit <- liftIO $ hReady stdin  -- Check if a key has been pressed
    if exit
        then do
            key <- getChar
            if key == '\ESC'
                then pure ()
                else runComputer computer
        else runComputer computer

readBinaryContent :: FilePath -> IO ByteString
readBinaryContent hackFile = withFile hackFile ReadMode BS.hGetContents

-- type ComputerState = (MemoryState, ScreenState, ROM32kState, CPURegisters, CPUInstruction, CPUInput)
loadMemory :: [HackWord16] -> IO ComputerState
loadMemory program = do
    let initialCpuInstruction = pure Zero
        cpuInput = pure Zero
        initialDRegister = pure Zero
        initialARegister = pure Zero
        initialPc = pure Zero

    memoryState <- random32KMemory
    screenState <- randomRam16K
    rom32kState <- addProgram program

    pure (memoryState, screenState, rom32kState, (initialARegister, initialDRegister, initialPc), initialCpuInstruction, cpuInput)

-- TODO: addProgram reads file and loads it onto memory
addProgram :: [HackWord16] -> IO ROM32kState
addProgram program = do
    initialRom <- random32KMemory
    let updatedRom = addProgramFoldl initialRom program
    pure updatedRom

type MemoryAddress = HackWord16
type ProgramData = HackWord16

addProgramFoldl :: ROM32kState -> [HackWord16] -> ROM32kState
addProgramFoldl initialRom progData = snd $ foldl foo (pure Zero, initialRom) progData
    where
        foo :: (MemoryAddress, ROM32kState) -> ProgramData -> (MemoryAddress, ROM32kState)
        foo (addr, romState) progData' = (inc16 addr, execState (loadROM32K addr progData') romState)
        

--TODO: delete them all
randomBit :: IO Bit
randomBit = pure Zero

-- random16Bits :: IO HackWord16
-- random16Bits = toHackWord16 <$> replicateM 16 randomBit

type Ram8State = Bus8Way HackWord16
randomRam8 :: IO Ram8State
randomRam8 = toBus8 <$> replicateM 8 randomInstruction

type Ram64State = Bus8Way Ram8State
randomRam64 :: IO Ram64State
randomRam64 = toBus8 <$> replicateM 8 randomRam8

type Ram512State = Bus8Way Ram64State
randomRam512 :: IO Ram512State
randomRam512 = toBus8 <$> replicateM 8 randomRam64

type Ram4KState = Bus8Way Ram512State
randomRam4K :: IO Ram4KState
randomRam4K = toBus8 <$> replicateM 8 randomRam512

-- randomRam8K :: IO (Bus2Way Ram4KState)
-- randomRam8K = toBus2 <$> replicateM 2 randomRam4K

type Ram16KState = Bus4Way Ram4KState
randomRam16K :: IO Ram16KState
randomRam16K = toBus4 <$> replicateM 4 randomRam4K

-- type ROM32kState = Bus2Way Ram16KState
random32KMemory :: IO ROM32kState
random32KMemory = toBus2 <$> replicateM 2 randomRam16K 

randomInstruction :: IO HackWord16
randomInstruction = do
                arand <- randomBit
                c0 <- randomBit
                c1 <- randomBit
                c2 <- randomBit
                c3 <- randomBit
                c4 <- randomBit
                c5 <- randomBit
                d0 <- randomBit
                d1 <- randomBit
                d2 <- randomBit
                -- j0 <- randomBit
                -- j1 <- randomBit
                -- j2 <- randomBit

                pure $ HackWord16F (One, One, One, arand, c0, c1, c2, c3, c4, c5, d0, d1, d2, Zero, Zero, Zero)