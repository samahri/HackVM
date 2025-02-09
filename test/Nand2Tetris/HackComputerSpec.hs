module Nand2Tetris.HackComputerSpec (
    spec
) where

import Test.Hspec

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Chips (inc16)
import Nand2Tetris.TestUtil
import Nand2Tetris.HackComputer

import BasicPrelude (($), (>>), pure)
import Control.Monad.Trans.State.Strict (execState, evalState, runState)
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

        context "implements an A-instruction" $ do
            it "Set the A-Register to @xxx" $ do
                mInput <- random16Bits
                instruction <- randomAInstruction
                
                initialDRegister <- random16Bits
                initialARegister <- random16Bits
                initialPc <- random16Bits

                let initialState = (initialARegister, initialDRegister, initialPc)
                    nextInstrAddr = inc16 initialPc

                execState (cpu mInput instruction Zero) initialState `shouldBe` (instruction, initialDRegister, nextInstrAddr)

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
                it "jumps if ALU out > 0" $ do

                    mInput <- random16Bits
                    bit1 <- randomBit
                    bit2 <- randomBit
                    bit3 <- randomBit
                    
                    let instruction = HackWord16F (One, One, One, a, c_1, c_1, c_1, c_1, c_1, c_1, bit1, bit2, bit3, Zero, Zero, One)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)

                    let (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState

                    pcReg `shouldBe` initialARegister

    specify "ram64K" pending
    specify "hackComputer" pending