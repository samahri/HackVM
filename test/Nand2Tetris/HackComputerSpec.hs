module Nand2Tetris.HackComputerSpec (
    spec
) where

import Test.Hspec

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.TestUtil
import Nand2Tetris.HackComputer

import BasicPrelude (($), (>>), pure)
import Control.Monad.Trans.State.Strict (execState, evalState)
import Control.Monad (replicateM_)


spec :: Spec
spec = do
    context "cpu" $ do
        context "implements an A-instruction" $ do
            it "Set the A-Register to @xxx" $ do
                mInput <- random16Bits
                instruction <- randomAInstruction
                
                let resetBit = Zero 

                initialDRegister <- random16Bits
                initialARegister <- random16Bits
                initialPc <- random16Bits

                let initialState = (initialARegister, initialDRegister, initialPc)

                execState (cpu mInput instruction resetBit) initialState `shouldBe` (instruction, initialDRegister, initialPc)

        context "implements a C-instruction" $ do
            context "that have no jumps" $ do
                let a = Zero
                    c0 = Zero
                    c1 = One
                    j = Zero
                context "ALU output is saved to..." $ do
                    it "the RAM" $ do
                        mInput <- random16Bits
                        bit1 <- randomBit
                        bit2 <- randomBit
                        
                        let instruction = HackWord16F (One, One, One, a, c1, c1, c1, c1, c1, c1, bit1, bit2, One, j, j, j)

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
                        
                        let instruction = HackWord16F (One, One, One, a, c1, c1, c1, c1, c1, c1, bit1, One, bit2, j, j, j)

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

                        let instruction = HackWord16F (One, One, One, a, c1, c1, c1, c1, c1, c1, One, bit1, bit2, j, j, j)

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
                    
                    let instruction = HackWord16F (One, One, One, a, c0, c0, c1, c1, c0, c0, bit1, bit2, One, j, j, j)

                    initialDRegister <- random16Bits
                    initialARegister <- random16Bits
                    initialPc <- random16Bits

                    let initialState = (initialARegister, initialDRegister, initialPc)
                    
                    let (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState

                    mOutput `shouldBe` initialDRegister 

    specify "ram64K" pending
    specify "hackComputer" pending