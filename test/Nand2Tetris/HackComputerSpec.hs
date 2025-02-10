module Nand2Tetris.HackComputerSpec (
    spec
) where

import Test.Hspec

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Chips (inc16)
import Nand2Tetris.TestUtil
import Nand2Tetris.HackComputer

import BasicPrelude (($), pure)
import Control.Monad.Trans.State.Strict (execState, evalState)
import Control.Monad (replicateM_)


spec :: Spec
spec = do
    context "cpu" $ do
        let a = Zero
            c_0 = Zero
            c_1 = One
            j = Zero

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

                let (aReg, dReg, _) = execState (cpu mInput instruction Zero) initialState

                (aReg, dReg) `shouldBe` (instruction, initialDRegister)

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

        context "implements a C-instruction" $ do
            context "that have no jumps" $ do
                it "increments to the next instruction" $ do
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    bit3 <- randomBit
                    
                    let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, bit3, j, j, j)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc

                    let (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState

                    pcReg `shouldBe` nextInstrAddr
                
                context "ALU output is saved to..." $ do
                    it "the RAM" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        
                        let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, One, j, j, j)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                        
                        let (_, mOutput, mwrite, _) = evalState (cpu mInput instruction Zero) initialState

                        (mOutput, mwrite) `shouldBe` (one, One)
                    
                    it "D register" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        
                        let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, One, bit2, j, j, j)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)

                        let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState
                            (_, dregister, _) = execState (cpu mInput instruction Zero) initialState

                        mOutput `shouldBe` one
                        dregister `shouldBe` one

                    it "A register" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit

                        let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, One, bit1, bit2, j, j, j)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)

                        let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState
                            (aregister, _, _) = execState (cpu mInput instruction Zero) initialState

                        mOutput `shouldBe` one
                        aregister `shouldBe` one
                it "ALU output the content of the D register" $ do

                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    
                    let instruction = HackWord16F (One, One, One, a, c_0, c_0, c_1, c_1, c_0, c_0, bit1, bit2, One, j, j, j)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                    
                    let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState

                    mOutput `shouldBe` initialDRegister 

                it "ALU output the content of the A register" $ do
                    
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    
                    let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_0, c_0, c_0, c_0, bit1, bit2, One, j, j, j)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                    
                    let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState

                    mOutput `shouldBe` initialARegister 
                
                it "ALU output the content of the M register" $ do
                    
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit

                    let a1 = One
                    
                    let instruction = HackWord16F (One, One, One, a1, c_1, c_1, c_0, c_0, c_0, c_0, bit1, bit2, One, j, j, j)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                    
                    let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState

                    mOutput `shouldBe` mInput 

            context "that have jumps" $ do
                it "jumps unconditionally" $ do
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    bit3 <- randomBit
                    
                    let instructionPos = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, bit3, One, One, One)
                        instructionZero = HackWord16F (One, One, One, a, c_1, c_0, c_1, c_0, c_1, c_0, bit1, bit2, bit3, One, One, One)
                        instructionNeg = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_0, c_1, c_0, bit1, bit2, bit3, One, One, One)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)

                    let (_, _, pcRegPos) = execState (cpu mInput instructionPos Zero) initialState
                        (_, _, pcRegZer) = execState (cpu mInput instructionZero Zero) initialState
                        (_, _, pcRegNeg) = execState (cpu mInput instructionNeg Zero) initialState
                        
                    pcRegPos `shouldBe` initialARegister
                    pcRegZer `shouldBe` initialARegister
                    pcRegNeg `shouldBe` initialARegister

                it "jumps if ALU < 0" $ replicateM_ 20 $ do
                    
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    bit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit
                    
                    let instructionNegJmp = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_0, c_1, c_0, bit1, bit2, bit3, One, jbit0, jbit1)
                        instructionNegInc = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_0, c_1, c_0, bit1, bit2, bit3, Zero, jbit0, jbit1)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc 

                    let (_, _, pcRegNegJmp) = execState (cpu mInput instructionNegJmp Zero) initialState
                        (_, _, pcRegNegInc) = execState (cpu mInput instructionNegInc Zero) initialState
                        

                    pcRegNegInc `shouldBe` nextInstrAddr
                    pcRegNegJmp `shouldBe` initialARegister

                it "jumps if ALU == 0" $ replicateM_ 20 $ do
                    
                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    bit3 <- randomBit

                    jbit0 <- randomBit
                    jbit1 <- randomBit
                    
                    let instructionZeroJmp = HackWord16F (One, One, One, a, c_1, c_0, c_1, c_0, c_1, c_0, bit1, bit2, bit3, jbit0,  One, jbit1)
                        instructionZeroInc = HackWord16F (One, One, One, a, c_1, c_0, c_1, c_0, c_1, c_0, bit1, bit2, bit3, jbit0, Zero, jbit1)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                        nextInstrAddr = inc16 initialPc 

                    let (_, _, pcRegZeroJmp) = execState (cpu mInput instructionZeroJmp Zero) initialState
                        (_, _, pcRegZeroInc) = execState (cpu mInput instructionZeroInc Zero) initialState
                        

                    pcRegZeroInc `shouldBe` nextInstrAddr
                    pcRegZeroJmp `shouldBe` initialARegister

                it "jumps if ALU out > 0" $ replicateM_ 20 $ do

                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        bit3 <- randomBit

                        jbit0 <- randomBit
                        jbit1 <- randomBit

                        let instructionPosJmp = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, bit3, jbit0, jbit1, One)
                            instructionPosInc = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, bit3, jbit0, jbit1, Zero)

                        initialDRegister <- random16Bits
                        initialARegister <- random16Bits
                        initialPc <- random16Bits

                        let initialState = (initialARegister, initialDRegister, initialPc)
                            nextInstrAddr = inc16 initialPc 

                        let (_, _, pcRegPosJmp) = execState (cpu mInput instructionPosJmp Zero) initialState
                            (_, _, pcRegPosInc) = execState (cpu mInput instructionPosInc Zero) initialState
                            
                        pcRegPosJmp `shouldBe` initialARegister
                        pcRegPosInc `shouldBe` nextInstrAddr

    specify "ram64K" pending
    specify "hackComputer" pending