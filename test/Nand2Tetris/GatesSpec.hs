module Nand2Tetris.GatesSpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16

import BasicPrelude (($))
import Nand2Tetris.Gates
import Test.Hspec
import Data.List (take, cycle, replicate)
import Control.Exception (evaluate)

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
        mux (Zero, One) One `shouldBe` One
        mux (Zero, One) Zero `shouldBe` Zero
    specify "mux16 gate" pending
    specify "dmux gate" pending
    describe "or8Way" $ do
        it "should only accept 8 bit arrays" pending
        specify "or8Way gate" pending
    describe "mux4Way16" $ do 
        it "should only accept 16 bit arrays" pending
        specify "mux4Way16" pending
    specify "dMux4Way" pending