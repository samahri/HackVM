module Nand2Tetris.GatesSpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.TestUtil
import Nand2Tetris.Types.Bus

import BasicPrelude (($))
import Nand2Tetris.Gates
import Test.Hspec
import Data.List (take, cycle, replicate)

spec :: Spec
spec = do
    let ones = toHackWord16 $ replicate 16 One 
        zeros = toHackWord16 $ replicate 16 Zero
    specify "nand gate" $ do
        nand (One, One)   `shouldBe` Zero
        nand (One, Zero)  `shouldBe` One
        nand (Zero, One)  `shouldBe` One
        nand (Zero, Zero) `shouldBe` One
    specify "not gate" $ do
        not One `shouldBe` Zero
        not Zero `shouldBe` One
    describe "not16 gate" $ do
        specify "not16 gate" $ do
            not16 ones `shouldBe` zeros
            not16 zeros `shouldBe` ones
            not16 (toHackWord16 [One, One, One, One, Zero, Zero, Zero, Zero, One, One, One, One, Zero, Zero, Zero, Zero])
                `shouldBe`
                toHackWord16 [Zero, Zero, Zero, Zero, One, One, One, One, Zero, Zero, Zero, Zero, One, One, One, One]
    specify "and gate" $ do
       and (One, One)   `shouldBe` One
       and (One, Zero)  `shouldBe` Zero
       and (Zero, One)  `shouldBe` Zero
       and (Zero, Zero) `shouldBe` Zero
    specify "and16 gate" $ do
        and16 (ones, zeros) `shouldBe` zeros
        and16 (ones, ones) `shouldBe` ones
        and16 (toHackWord16 $ take 16 $ cycle [One, Zero], toHackWord16 $ take 16 $ cycle [Zero, One]) `shouldBe` zeros
    specify "or gate" $ do
        or (One, One)   `shouldBe` One
        or (One, Zero)  `shouldBe` One
        or (Zero, One)  `shouldBe` One
        or (Zero, Zero) `shouldBe` Zero
    specify "or16 gate" $ do
        or16 (zeros, zeros) `shouldBe` zeros
        or16 (ones, zeros) `shouldBe` ones
        or16 (toHackWord16 $ take 16 $ cycle [One, Zero], toHackWord16 $ take 16 $ cycle [Zero, One]) `shouldBe` ones 
    specify "nor gate" $ do
        nor (One, One)   `shouldBe` Zero
        nor (One, Zero)  `shouldBe` Zero
        nor (Zero, One)  `shouldBe` Zero
        nor (Zero, Zero) `shouldBe` One
    specify "xor gate" $ do
        xor (One, One)   `shouldBe` Zero
        xor (One, Zero)  `shouldBe` One
        xor (Zero, One)  `shouldBe` One
        xor (Zero, Zero) `shouldBe` Zero
    
    specify "mux gate" $ do
        bit0 <- randomBit
        bit1 <- randomBit

        mux (bit0, bit1) Zero `shouldBe` bit0
        mux (bit0, bit1) One `shouldBe` bit1

    specify "mux16 gate" $ do
        bit0 <- random16Bits
        bit1 <- random16Bits

        mux16 (bit0, bit1) Zero `shouldBe` bit0
        mux16 (bit0, bit1) One `shouldBe` bit1

    specify "dmux gate" $ do
        bit0 <- randomBit
        
        dmux bit0 Zero `shouldBe` (bit0, Zero)
        dmux bit0 One `shouldBe` (Zero, bit0)
    
    specify "or8Way gate" pending
    
    specify "mux4Way16" $ do
        bit0 <- random16Bits
        bit1 <- random16Bits
        bit2 <- random16Bits
        bit3 <- random16Bits

        mux4Way16 (Bus4Way (bit0, bit1, bit2, bit3)) (Zero, Zero) `shouldBe` bit0
        mux4Way16 (Bus4Way (bit0, bit1, bit2, bit3)) (Zero, One) `shouldBe` bit1
        mux4Way16 (Bus4Way (bit0, bit1, bit2, bit3)) (One, Zero) `shouldBe` bit2
        mux4Way16 (Bus4Way (bit0, bit1, bit2, bit3)) (One, One) `shouldBe` bit3
    
    specify "mux8Way16" $ do
        bit0 <- random16Bits
        bit1 <- random16Bits
        bit2 <- random16Bits
        bit3 <- random16Bits
        bit4 <- random16Bits
        bit5 <- random16Bits
        bit6 <- random16Bits
        bit7 <- random16Bits

        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (Zero, Zero, Zero) `shouldBe` bit0
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (Zero, Zero, One) `shouldBe` bit1
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (Zero, One, Zero) `shouldBe` bit2
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (Zero, One, One) `shouldBe` bit3
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (One, Zero, Zero) `shouldBe` bit4
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (One, Zero, One) `shouldBe` bit5
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (One, One, Zero) `shouldBe` bit6
        mux8Way16 (Bus8Way (bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)) (One, One, One) `shouldBe` bit7

    specify "dMux4Way" $ do
        bit0 <- randomBit

        dMux4Way bit0 (Zero, Zero) `shouldBe` Bus4Way (bit0, Zero, Zero, Zero)
        dMux4Way bit0 (Zero, One) `shouldBe` Bus4Way (Zero, bit0, Zero, Zero)
        dMux4Way bit0 (One, Zero) `shouldBe` Bus4Way (Zero, Zero, bit0, Zero)
        dMux4Way bit0 (One, One) `shouldBe` Bus4Way (Zero, Zero, Zero, bit0)

    specify "dMux8Way" pending

    specify "dMux4Way16" $ do
        bit0 <- random16Bits

        dMux4Way16 bit0 (Zero, Zero) `shouldBe` Bus4Way (bit0, zeros, zeros, zeros)
        dMux4Way16 bit0 (Zero, One) `shouldBe` Bus4Way (zeros, bit0, zeros, zeros)
        dMux4Way16 bit0 (One, Zero) `shouldBe` Bus4Way (zeros, zeros, bit0, zeros)
        dMux4Way16 bit0 (One, One) `shouldBe` Bus4Way (zeros, zeros, zeros, bit0)
    
    specify "dMux8Way16" $ do
        bit0 <- random16Bits

        dMux8Way16 bit0 (Zero, Zero, Zero) `shouldBe` Bus8Way (bit0, zeros, zeros, zeros, zeros, zeros, zeros, zeros)
        dMux8Way16 bit0 (Zero, Zero, One) `shouldBe` Bus8Way (zeros, bit0, zeros, zeros, zeros, zeros, zeros, zeros)
        dMux8Way16 bit0 (Zero, One, Zero) `shouldBe` Bus8Way (zeros, zeros, bit0, zeros, zeros, zeros, zeros, zeros)
        dMux8Way16 bit0 (Zero, One, One) `shouldBe` Bus8Way (zeros, zeros, zeros, bit0, zeros, zeros, zeros, zeros)
        dMux8Way16 bit0 (One, Zero, Zero) `shouldBe` Bus8Way (zeros, zeros, zeros, zeros, bit0, zeros, zeros, zeros)
        dMux8Way16 bit0 (One, Zero, One) `shouldBe`  Bus8Way (zeros, zeros, zeros, zeros, zeros, bit0, zeros, zeros)
        dMux8Way16 bit0 (One, One, Zero) `shouldBe` Bus8Way (zeros, zeros, zeros, zeros, zeros, zeros, bit0, zeros)
        dMux8Way16 bit0 (One, One, One) `shouldBe` Bus8Way (zeros, zeros, zeros, zeros, zeros, zeros, zeros, bit0)


