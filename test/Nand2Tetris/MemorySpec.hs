{-# LANGUAGE ExtendedDefaultRules #-}
module Nand2Tetris.MemorySpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.TestUtil
import Control.Monad.Trans.State.Strict (evalState)
import BasicPrelude (($), (<$>), (>>), IO, Int, (^), (+), (==), foldr, fst, mod, pure)
import Nand2Tetris.Memory
import Test.Hspec
import Data.List (replicate)
import Control.Monad (replicateM_)

spec :: Spec
spec = do
    
    specify "flip flop" $ replicateM_ 20 $ do
        bit1 <- randomBit
        bit2 <- randomBit
        initialBit <- randomBit

        outputbit <- randomBit

        -- first dff call does not affect output
        evalState (dff bit1 >> dff outputbit >> dff bit2) initialBit `shouldBe` outputbit

    context "1-bit register" $ do
        specify "load is disabled" $ replicateM_ 20 $ do
            inputbit <- randomBit
            initialBit <- randomBit
            bit1 <- randomBit
            bit2 <- randomBit
            bit3 <- randomBit

            -- when load is disabled, only the initial State is preserved and subsequent reads do not update the state
            evalState (bit inputbit One >> bit bit1 Zero >> bit bit2 bit3) initialBit `shouldBe` inputbit
        
        specify "load is enabled" $ replicateM_ 20 $ do
            -- only the last two updates matter
            -- when load is enabled, it returns the previous input; the state is simply the input before that
            inputbit <- randomBit
            initialBit <- randomBit

            bit1 <- randomBit
            bit2 <- randomBit
            evalState (bit bit1 One >> bit inputbit One >> bit bit1 bit2) initialBit `shouldBe` inputbit

    context "16-bit register" $ do
        specify "load is disabled" $ replicateM_ 40 $ do
            inputbits <- random16Bits
            initialBits <- random16Bits

            bit1 <- randomBit
            bits0 <- random16Bits
            bits1 <- random16Bits

            evalState (register inputbits One >> register bits0 Zero >> register bits1 bit1) initialBits `shouldBe` inputbits
        
        specify "load is enabled" $ replicateM_ 40 $ do
            inputbits <- random16Bits
            initialBits <- random16Bits
            bit1 <- randomBit
            bits0 <- random16Bits
            bits1 <- random16Bits 
            
            evalState (register bits0 One >> register inputbits One >> register bits1 bit1) initialBits `shouldBe`  inputbits

    context "program counter" $ do

        it "initializes to zeros" $ replicateM_ 40 $ do
            let zeros = toHackWord16 $ replicate 16 Zero

            inputbits <- random16Bits
            initialBits <- random16Bits
            bits0 <- random16Bits

            bit1 <- randomBit
            bit2 <- randomBit
            bit3 <- randomBit

            evalState (pc inputbits (Zero, Zero, One) >> pc bits0 (bit1, bit2, bit3)) initialBits `shouldBe` zeros
        
        it "initializes to input register" $ replicateM_ 40 $ do
            inputbits <- random16Bits
            initialBits <- random16Bits
            bits0 <- random16Bits
            
            bit1 <- randomBit
            bit2 <- randomBit
            bit3 <- randomBit

            evalState (pc inputbits (One, Zero, Zero) >> pc bits0 (bit1, bit2, bit3)) initialBits `shouldBe` inputbits
        
        it "output past state when no controls are added" $ replicateM_ 40 $ do
            inputbits <- random16Bits
            initialBits <- random16Bits

            bits0 <- random16Bits
            bits1 <- random16Bits
            
            bit1 <- randomBit
            bit2 <- randomBit
            bit3 <- randomBit

            evalState (pc inputbits (One, Zero, Zero) >> pc bits0 (Zero, Zero, Zero) >> pc bits1 (bit1, bit2, bit3)) initialBits `shouldBe` inputbits
       
        it "increments by two" $ replicateM_ 40 $ do
            
            inputbits <- random16Bits
            initialBits <- random16Bits

            bits0 <- random16Bits
            bits1 <- random16Bits
            bits2 <- random16Bits
            
            bit1 <- randomBit
            bit2 <- randomBit
            bit3 <- randomBit
            let inputInt = convertToInt $ toList inputbits
                outputInt = convertToInt $ toList $ evalState (pc inputbits (One, Zero, Zero) >> pc bits0 (Zero, One, Zero) >> pc bits1 (Zero, One, Zero) >> pc bits2 (bit1, bit2, bit3)) initialBits

            outputInt `shouldBe` (inputInt + 2) `mod` 256
    
    context "RAM8" $  do
        let getAddress :: IO (Bit, Bit, Bit)
            getAddress = do
                addrBit0 <- randomBit
                addrBit1 <- randomBit
                addrBit2 <- randomBit
                -- reserve (Zero, One, Zero) to prevent addr2 from being overritten
                if 
                    (addrBit0, addrBit1, addrBit2) == (Zero, One, Zero) 
                    then getAddress 
                    else pure (addrBit0, addrBit1, addrBit2)

        it "writes and reads an 8-bit number" $ replicateM_ 40 $ do
            
            inputbits1 <- random16Bits
            inputbits2 <- random16Bits
            inputbits3 <- random16Bits

            initialBits <- replicate 8 <$> random16Bits
            
            addr1 <- getAddress
            addr2 <- getAddress

            bits0 <- random16Bits

            evalState (ram8 inputbits1 addr1 One  >> ram8 inputbits2 addr2 One  >> ram8 inputbits3 (Zero, One, Zero) One  >> ram8 bits0 addr2 Zero) initialBits `shouldBe` inputbits2

    -- context "RAM64" $ do
    --     it "writes and reads an 8-bit number" $ do
    --         inputbits1 <- random16Bits
    --         inputbits2 <- random16Bits
    --         inputbits3 <- random16Bits

    --         initialBits <- replicate 8 <$> random16Bits
            
    --         addr1 <- getAddress
    --         addr2 <- getAddress

    --         bits0 <- random16Bits

    --         evalState (ram64 inputbits1 addr1 One  >> ram8 inputbits2 addr2 One  >> ram8 inputbits3 (Zero, One, Zero) One  >> ram8 bits0 addr2 Zero) initialBits `shouldBe` inputbits2


convertToInt :: [Bit] -> Int
convertToInt input = fst (foldr func (0, 0) input) `mod` 256
    where
        func :: Bit -> (Int, Int) -> (Int, Int)
        func b (total, acc) = if b == Zero then (total, acc + 1) else (total + 2^acc, acc + 1)