module Nand2Tetris.HackComputerSpec (
    spec
) where

import Test.Hspec

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Gates (mux16)
import Nand2Tetris.Chips (inc16, alu, AluCtrl(..))
import Nand2Tetris.Memory (rom32K)
import Nand2Tetris.TestUtil
import Nand2Tetris.HackComputer

import BasicPrelude (($), pure, (>>), (.), reverse, (<$>))
import Control.Monad.Trans.State.Strict (execState, evalState, execStateT, evalStateT)
import Control.Monad (replicateM_)

spec :: Spec
spec = do
    context "cpu" $ do

        let randomInstruction = do
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
                j0 <- randomBit
                j1 <- randomBit
                j2 <- randomBit

                pure $ HackWord16F (One, One, One, arand, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2)

        it "resets the CPU when reset flag is enabled" $ do
            mInput <- random16Bits

            instruction <- randomInstruction

            initialDRegister <- random16Bits
            initialARegister <- random16Bits
            initialPc <- random16Bits

            let initialState = (initialARegister, initialDRegister, initialPc)
            
            let (_, _, pcReg) = execState (cpu mInput instruction One) initialState

            pcReg `shouldBe` zeros
        
        it "Writes the content to A Register to writeM" $ do
            mInput <- random16Bits

            instruction <- randomInstruction

            initialDRegister <- random16Bits
            initialARegister <- random16Bits
            initialPc <- random16Bits

            let initialState = (initialARegister, initialDRegister, initialPc)
            
            let (mAddress, _, _, _) = evalState (cpu mInput instruction One) initialState

            mAddress `shouldBe` initialARegister 

        context "implements an A-instruction" $ do
            
            it "Set the A-Register to @xxx" $ do
                mInput <- random16Bits
                instruction <- randomAInstruction
                
                initialDRegister <- random16Bits
                initialARegister <- random16Bits
                initialPc <- random16Bits

                let initialState = (initialARegister, initialDRegister, initialPc)

                let (aReg, _, _) = execState (cpu mInput instruction Zero) initialState

                aReg `shouldBe` instruction

            it "increments to the next instruction" $ do
                    mInput <- random16Bits
                    instruction <- randomAInstruction

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc

                    let (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState

                    pcReg `shouldBe` nextInstrAddr
            
            it "D register remain unchanged" $ do
                mInput <- random16Bits
                instruction <- randomAInstruction
                
                initialDRegister <- random16Bits
                initialARegister <- random16Bits
                initialPc <- random16Bits

                let initialState = (initialARegister, initialDRegister, initialPc)

                let (_, dReg, _) = execState (cpu mInput instruction Zero) initialState

                dReg `shouldBe` initialDRegister

        context "implements a C-instruction" $ do

            context "Stores comp to..." $ do
                context "the RAM" $ do
                    
                    it "mOutput is 1 when dd1" $ replicateM_ 10 $ do
                        mInput <- random16Bits

                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit
                        
                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                                zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                                bit1, bit2, One, 
                                                jbit0, jbit1, jbit2)
                            
                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (_, _, mwrite, _) = evalState (cpu mInput instruction Zero) initialState

                        mwrite `shouldBe` One

                    it "mOutput is 0 when dd0" $ replicateM_ 10 $ do
                        mInput <- random16Bits

                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit

                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                                zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                                bit1, bit2, Zero, 
                                                jbit0, jbit1, jbit2)
                            
                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (_, _, mwrite, _) = evalState (cpu mInput instruction Zero) initialState

                        mwrite `shouldBe` Zero
                
                context "D register" $ do

                    it "equals mOutput when d1d" $ do
                        mInput <- random16Bits
                        
                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit 
                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                            zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                            bit1, One ,bit2,
                                            jbit0, jbit1, jbit2)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState
                            (_, dregister, _) = execState (cpu mInput instruction Zero) initialState

                        dregister `shouldBe` mOutput
                    
                    it "unchanged when d0d" $ do
                        mInput <- random16Bits
                        
                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit  
                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                            zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                            bit1, Zero ,bit2,
                                            jbit0, jbit1, jbit2)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (_, dregister, _) = execState (cpu mInput instruction Zero) initialState

                        dregister `shouldBe` initialDRegister

                context "A register" $ do
                    it "equals mOutput when 1dd" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit  
                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                            zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                            One, bit1, bit2,
                                            jbit0, jbit1, jbit2)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState
                            (aregister, _, _) = execState (cpu mInput instruction Zero) initialState

                        aregister `shouldBe` mOutput

                    it "unchanged when 0dd" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        jbit0 <- randomBit
                        jbit1 <- randomBit
                        jbit2 <- randomBit  

                        (a, randomAluCtrl) <- getRandomAluCtrl
                    
                        let instruction = HackWord16F (One, One, One, a, 
                                            zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                            Zero, bit1, bit2,
                                            jbit0, jbit1, jbit2)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            (aregister, _, _) = execState (cpu mInput instruction Zero) initialState

                        aregister `shouldBe` initialARegister

            it "mOutput == comp" $ replicateM_ 10 $ do
                mInput <- random16Bits
                dbit1 <- randomBit
                dbit2 <- randomBit
                dbit3 <- randomBit

                jbit1 <- randomBit
                jbit2 <- randomBit
                jbit3 <- randomBit
                
                (a, randomAluCtrl) <- getRandomAluCtrl
                    
                let instruction = HackWord16F (One, One, One, a, 
                                    zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    jbit1, jbit2, jbit3)
                    

                initialDRegister <- random16Bits
                initialARegister <- random16Bits
                initialPc <- random16Bits

                let initialState = (initialARegister, initialDRegister, initialPc)
                    (aluOutput, _, _) = alu (initialDRegister, mux16 (initialARegister, mInput) a) randomAluCtrl
                
                let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState

                mOutput `shouldBe` aluOutput 
 
            context "that have jumps" $ do
                let aluOutputNegativeOne = (Zero, AluCtrl {zx = One, nx = One,  zy =One, ny = Zero, f = One, no = Zero})
                    aluOutputZero = (Zero, AluCtrl {zx = One, nx = Zero, zy = One, ny = Zero, f = One, no = Zero})
                    aluOutputOne = (Zero, AluCtrl {zx = One, nx = One,  zy =One,  ny = One, f = One, no = One})

                it "jumps if ALU < 0" $ do
                    
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputNegativeOne
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    One, jbit0, jbit1)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState
                    
                    pcReg `shouldBe` initialARegister

                it "increments when ALU < 0" $ do
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputNegativeOne
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    Zero, jbit0, jbit1)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc 

                    let (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState

                    pcReg `shouldBe` nextInstrAddr

                it "jumps if ALU == 0" $ do
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputZero
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    jbit0, One, jbit1)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState
                    
                    pcReg `shouldBe` initialARegister
                
                it "increments if ALU == 0" $ do
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputZero
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    jbit0, Zero, jbit1)
                    
                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc  

                    let (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState
                    
                    pcReg `shouldBe` nextInstrAddr
                
                it "jumps if ALU out > 0" $ do
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputOne
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    jbit0, jbit1, One)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState
                    
                    pcReg `shouldBe` initialARegister

                it "increments if ALU > 0" $ do
                    mInput <- random16Bits
                    dbit1 <- randomBit
                    dbit2 <- randomBit
                    dbit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit

                    (a, aluCtrl) <- pure aluOutputOne
                    
                    let instruction = HackWord16F (One, One, One, a, 
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl , 
                                    dbit1, dbit2, dbit3,
                                    jbit0, jbit1, Zero)
                    
                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc 
                        (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState

                    pcReg `shouldBe` nextInstrAddr 

    context "mainMemory" $ do
        it "reads from the ram address space" pending

    context "hackComputer" $ do
        {--
            forall (computer :: Reset -> HackComputer, 
                    rom :: ROMAddress -> ROM32kOutput, 
                    x :: HackWord16,
                    r :: Reset).
                rom 0x_000 0000 0000 0000 = x -> cpuInstruction (execState (computer 1 >> computer r)) == x
        --}
        it "ROM points to address zero when computer is reset" $ do
            
            initialCpuInstruction <- random16Bits
            cpuInput <- random16Bits
            memoryState <- random32KMemory
            ram16kState <- randomRam16K
            rom32kState <- random32KMemory

            initialDRegister <- random16Bits
            initialARegister <- random16Bits
            initialPc <- random16Bits

            bit <- randomBit

            -- type ComputerState = (MemoryState, Ram16kState, ROM32kState, CpuRegisters, CpuInstruction, CpuInput)
            let initialState = (memoryState, ram16kState, rom32kState, (initialARegister, initialDRegister, initialPc), initialCpuInstruction, cpuInput)
                address0 = pure Zero :: HackWord16
                romValAtAddr0 = evalState (rom32K address0) rom32kState
            
            (_, _, _, _, cpuInstruction, _) <- execStateT (hackComputer One >> hackComputer bit) initialState

            cpuInstruction `shouldBe` romValAtAddr0

        {--
            forall (computer :: Reset -> HackComputer,
                    mainMemory :: MemoryAddress -> MemoryOutput
                    addressM :: MemoryAddress,
                    x :: HackWord16 ).
                mainMemory addressM = x -> cpuInput (execState (computer 0)) == x
        --}
        it "cpu input = memory[addressM]" $ do

            cpuInstruction <- random16Bits
            initialCpuInput <- random16Bits
            memoryState <- random32KMemory
            ram16kState <- randomRam16K
            rom32kState <- random32KMemory

            addressM <- toHackWord16 . replaceHead . toList <$> random16Bits

            randomInput <- random16Bits
            loadBit <- randomBit

            initialDRegister <- random16Bits
            let initialARegister = addressM
            initialPc <- random16Bits
            
            let initialState = (memoryState, ram16kState, rom32kState, (initialARegister, initialDRegister, initialPc), cpuInstruction, initialCpuInput)
            
            memoryOutput <- (mainMemory addressM randomInput loadBit `evalStateT` memoryState) `evalStateT` ram16kState

            (_, _, _, _, _, cpuInput) <- execStateT (hackComputer Zero) initialState

            cpuInput `shouldBe` memoryOutput


replaceHead :: [Bit] -> [Bit]
replaceHead [] = []
replaceHead (_:xs) = reverse (Zero : xs)




