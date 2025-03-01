{-# LANGUAGE OverloadedStrings #-}
module CLI.HackComputer (
    main
) where

import BasicPrelude hiding (putStrLn)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict (evalStateT, execState, execStateT)
import System.IO (hSetBuffering, hSetEcho, stdin, hReady, BufferMode( NoBuffering ), withFile, IOMode(ReadMode), putStrLn)
import System.Exit(exitFailure)

import Nand2Tetris.Utils
import Nand2Tetris.HackComputer
import Nand2Tetris.Memory
import Nand2Tetris.Chips
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Memory
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Assembler
import CLI.Utils

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
    liftIO $ threadDelay 1000
    computer Zero
    exit <- liftIO $ hReady stdin  -- Check if a key has been pressed
    if exit
        then do
            key <- getChar
            if key == '\ESC'
                then pure ()
                else runComputer computer
        else runComputer computer

-- TODO: duplicate function from CLI.Assembler
readBinaryContent :: FilePath -> IO String
readBinaryContent hackFile = withFile hackFile ReadMode (readContent "" (<>))

loadMemory :: [HackWord16] -> IO ComputerState
loadMemory program = do
    initialCpuInstruction <- zero16Bits
    cpuInput <- zero16Bits
    initialDRegister <- zero16Bits
    initialARegister <- zero16Bits
    initialPc <- zero16Bits

    memoryState <- zero32KMemory
    screenState <- zeroRam16K
    rom32kState <- addProgram program

    pure (memoryState, screenState, rom32kState, (initialARegister, initialDRegister, initialPc), initialCpuInstruction, cpuInput)

-- TODO: addProgram reads file and loads it onto memory
addProgram :: [HackWord16] -> IO ROM32kState
addProgram program = do
    initialRom <- zero32KMemory
    let updatedRom = addProgramFoldl initialRom program
    pure updatedRom

type MemoryAddress = HackWord16
type ProgramData = HackWord16

addProgramFoldl :: ROM32kState -> [HackWord16] -> ROM32kState
addProgramFoldl initialRom progData = snd $ foldl foldFunc (pure Zero, initialRom) progData
    where
        foldFunc :: (MemoryAddress, ROM32kState) -> ProgramData -> (MemoryAddress, ROM32kState)
        foldFunc (addr, romState) progData' = (inc16 addr, execState (loadROM32K addr progData') romState)
        

zeroBit :: IO Bit
zeroBit = pure Zero

zero16Bits :: IO HackWord16
zero16Bits = toHackWord16 <$> replicateM 16 zeroBit

zeroRam8 :: IO RAM8State
zeroRam8 = toBus8 <$> replicateM 8 zero16Bits

zeroRam64 :: IO RAM64State
zeroRam64 = toBus8 <$> replicateM 8 zeroRam8

zeroRam512 :: IO RAM512State
zeroRam512 = toBus8 <$> replicateM 8 zeroRam64

zeroRam4K :: IO RAM4kState
zeroRam4K = toBus8 <$> replicateM 8 zeroRam512

zeroRam16K :: IO RAM16kState
zeroRam16K = toBus4 <$> replicateM 4 zeroRam4K

zero32KMemory :: IO ROM32kState
zero32KMemory = toBus2 <$> replicateM 2 zeroRam16K 