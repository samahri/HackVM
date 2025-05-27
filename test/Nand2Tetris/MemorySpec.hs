{-# LANGUAGE ExtendedDefaultRules #-}
module Nand2Tetris.MemorySpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.TestUtil ( zeros )
import Control.Monad.Trans.State.Strict (evalState, runState)
import BasicPrelude ( ($), (>>), (==), (/=), mod, (+) )
import Nand2Tetris.Memory
    ( bit, dff, pc, ram16K, ram4K, ram512, ram64, ram8, register )
import Nand2Tetris.Types.HackWord16 ( convertToInt )
import Test.Hspec ( context, it, pending, specify, Spec )
import Test.QuickCheck ( Testable(property), (==>) )

spec :: Spec
spec = do
    specify "flip flop" $ 
        property $ \bit1 initialBit outputBit -> runState (dff outputBit >> dff bit1) initialBit == (outputBit, bit1)

    context "1-bit register" $ do
        specify "load is disabled" $ 
            property $ \inputbit bit2 bit3 initialBit -> evalState (bit inputbit Zero >> bit bit2 bit3) initialBit == initialBit
        specify "load is enabled" $
            property $ \bit1 inputbit bit2 initialBit -> evalState (bit inputbit One >> bit bit1 bit2) initialBit == inputbit

    context "16-bit register" $ do
        specify "load is disabled" $
            property $ \inputbits rbits rbit initialBits -> evalState (register inputbits Zero >> register rbits rbit) initialBits == initialBits
        specify "load is enabled" $ 
            property $ \inputbits rbits rbit initialBits -> evalState (register inputbits One >> register rbits rbit) initialBits ==  inputbits

    context "program counter" $ do
        it "initializes to zeros" $
            property $ \inputbits bit1 bit2 bit3 bits0 initialBits -> evalState (pc inputbits (Zero, Zero, One) >> pc bits0 (bit1, bit2, bit3)) initialBits == zeros
        it "initializes to input register" $
            property $ \inputbits bit1 bit2 bit3 bits0 initialBits -> evalState (pc inputbits (One, Zero, Zero) >> pc bits0 (bit1, bit2, bit3)) initialBits == inputbits
        it "output past state when no controls are added" $
            property $ \inputbits bit1 bit2 bit3 bits0 initialBits -> evalState (pc inputbits (Zero, Zero, Zero) >> pc bits0 (bit1, bit2, bit3)) initialBits == initialBits
        it "increments by two" $
            property $ \inputbits bit1 bit2 bit3 bits0 bits1 bits2 initialBits -> 
                let inputInt = convertToInt inputbits
                    outputInt = convertToInt $ evalState (pc inputbits (One, Zero, Zero) >> pc bits0 (Zero, One, Zero) >> pc bits1 (Zero, One, Zero) >> pc bits2 (bit1, bit2, bit3)) initialBits
                in
                    outputInt == (inputInt + 2) `mod` 256
    
    context "RAM8" $  do
        it "writes and reads an 16-bit number" $
             property $ \addr1 inputbits1 addr2 addr3 inputbits2 inputbits3 bits0 initialBits 
                -> (addr2 /= addr3) ==> evalState (ram8 addr1 inputbits1 One  >> ram8 addr2 inputbits2 One  >> ram8 addr3 inputbits3 One  >> ram8 addr2 bits0 Zero) initialBits == inputbits2

    context "RAM64" $ do
        it "writes and reads an 16-bit number" $ 
            property $ \addr1 inputbits1 addr2 addr3 inputbits2 inputbits3 bits0 initialBits ->
                (addr2 /= addr3) ==> evalState (ram64 addr1 inputbits1 One  >> ram64 addr2 inputbits2 One  >> ram64 addr3 inputbits3 One  >> ram64 addr2 bits0 Zero) initialBits == inputbits2
    
    context "RAM512" $ do
        it "writes and reads an 16-bit number" $
            property $ \addr1 inputbits1 addr2 addr3 inputbits2 inputbits3 bits0 initialBits -> (addr2 /= addr3) ==> evalState (ram512 addr1 inputbits1 One  >> ram512 addr2 inputbits2 One  >> ram512 addr3 inputbits3 One  >> ram512 addr2 bits0 Zero) initialBits == inputbits2

    context "RAM4K" $ do
        it "writes and reads an 16-bit number" $
            property $ \addr1 inputbits1 addr2 addr3 inputbits2 inputbits3 bits0 initialBits -> (addr2 /= addr3) ==> evalState (ram4K addr1 inputbits1 One  >> ram4K addr2 inputbits2 One  >> ram4K addr3 inputbits3 One  >> ram4K addr2 bits0 Zero) initialBits == inputbits2
    
    context "RAM16K" $ do
        it "writes and reads an 16-bit number" $
            property $ \addr1 inputbits1 addr2 addr3 inputbits2 inputbits3 bits0 initialBits -> (addr2 /= addr3) ==> evalState (ram16K addr1 inputbits1 One  >> ram16K addr2 inputbits2 One >> ram16K addr3 inputbits3 One  >> ram16K addr2 bits0 Zero) initialBits == inputbits2

    context "rom32K" $ do
        -- let getRandomRomAddress = do
        --         addrBit0 <- randomBit
        --         addrBit1 <- randomBit
        --         addrBit2 <- randomBit
        --         addrBit3 <- randomBit
        --         addrBit4 <- randomBit
        --         addrBit5 <- randomBit
        --         addrBit6 <- randomBit
        --         addrBit7 <- randomBit
        --         addrBit8 <- randomBit
        --         addrBit9 <- randomBit
        --         addrBit10 <- randomBit
        --         addrBit11 <- randomBit
        --         addrBit12 <- randomBit
        --         addrBit13 <- randomBit 
        --         addrBit14 <- randomBit 
        --         addrBit15 <- randomBit 

        --         pure (addrBit0, addrBit1, addrBit2, addrBit3, addrBit4, addrBit5, addrBit6, addrBit7, addrBit8, addrBit9, addrBit10, addrBit11, addrBit12, addrBit13, addrBit14, addrBit15)
            
        it "reads a 16-bit number" pending
