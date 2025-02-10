module Nand2Tetris.ChipsSpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.TestUtil
import BasicPrelude (($), (<>), pure)
import Nand2Tetris.Chips
import Test.Hspec
import Data.List (replicate)

spec :: Spec
spec = do
    let ones = pure One 
        two = toHackWord16 $ replicate 14 Zero <> [One, Zero]
        neg2 = toHackWord16 $ replicate 15 One <> [Zero] -- 1111 1111 1111 1110
        num255 = toHackWord16 $ replicate 8 Zero <> replicate 8 One -- 0000 0000 1111 1111
        neg256 = toHackWord16 $ replicate 8 One <> replicate 8 Zero -- 1111 1111 0000 0000
    
    specify "half adder" $ do
        halfAdder (Zero, Zero)  `shouldBe` (Zero, Zero)
        halfAdder (Zero, One)   `shouldBe` (Zero, One)
        halfAdder (One, Zero)   `shouldBe` (Zero, One)
        halfAdder (One, One)    `shouldBe` (One, Zero)
    
    specify "full adder" $ do
        fullAdder (Zero, Zero, Zero) `shouldBe` (Zero, Zero)  
        fullAdder (Zero, Zero, One)  `shouldBe` (Zero, One)
        fullAdder (Zero, One, Zero)  `shouldBe` (Zero, One)
        fullAdder (Zero, One, One)   `shouldBe` (One, Zero)
        fullAdder (One, Zero, Zero)  `shouldBe` (Zero, One)
        fullAdder (One, Zero, One)   `shouldBe` (One, Zero)
        fullAdder (One, One, Zero)   `shouldBe` (One, Zero)
        fullAdder (One, One, One)    `shouldBe` (One, One)
    
    specify "16-bit adder" $ do
        -- TODO: random number generator
        add16 (zeros, zeros)    `shouldBe` zeros
        add16 (zeros, one)      `shouldBe` one
        add16 (neg2, one)       `shouldBe` ones
        add16 (ones, one)       `shouldBe` zeros
    
    context "incrementer" $ do
        specify "inc16" $ do
            let num256 = toHackWord16 $ replicate 7 Zero <> [One] <> replicate 8 Zero -- 0000 0001 0000 0
                num = toHackWord16 $ replicate 15 One <> [Zero]
            inc16 ones      `shouldBe` zeros
            inc16 zeros     `shouldBe` one
            inc16 num255    `shouldBe` num256 -- 0000 0000 1111 1111 -> 0000 0001 0000 0000
            inc16 num       `shouldBe` ones
    
    specify "ALU" $ do
        -- a = 0; c = 101010
        alu (zeros, ones) (One, Zero, One, Zero, One, Zero)     `shouldBe` (zeros, One, Zero)           

        -- zx=1, nx=1, zy=1, ny=1, f=1, no=1 -> 1
        alu (zeros, ones) (One, One, One, One, One, One)        `shouldBe` (one, Zero, Zero)       

        -- zx=1, nx=1, zy=1, ny=0, f=1, no=0 -> -1        
        alu (num255, num255) (One, One, One, Zero, One, Zero)   `shouldBe` (ones, Zero, One)        
        
        alu (num255, zeros)  (Zero, Zero, One, One, Zero, Zero)   `shouldBe` (num255, Zero, Zero)  -- zx=0, nx=0, zy=1, ny=1, f=0, no=0 -> x
        
        -- 
        alu (zeros, num255)  (One, One, Zero, Zero, Zero, Zero)   `shouldBe` (num255, Zero, Zero)  -- zx=1, nx=1, zy=0, ny=0, f=0, no=0 -> y
        alu (one, zeros)     (Zero, Zero, One, One, Zero, One)    `shouldBe` (neg2, Zero, One)  -- zx=0, nx=0, zy=1, ny=1, f=0, no=1 -> NOT x
        alu (zeros, num255)  (One, One, Zero, Zero, Zero, One)    `shouldBe` (neg256, Zero, One)  -- zx=1, nx=1, zy=0, ny=0, f=0, no=1 -> NOT y
        alu (one, zeros)     (Zero, Zero, One, One, One, One)     `shouldBe` (ones, Zero, One)     -- zx=0, nx=0, zy=1, ny=1, f=1, no=1 -> -x
        alu (zeros, one)     (One, One, Zero, Zero, One, One)     `shouldBe` (ones, Zero, One)     -- zx=1, nx=1, zy=0, ny=0, f=1, no=1 -> -y
        alu (zeros, zeros)   (Zero, One, One, One, One, One)      `shouldBe` (one, Zero, Zero)    -- zx=0, nx=1, zy=1, ny=1, f=1, no=1 -> x + 1
        alu (zeros, one)     (One, One, Zero, One, One, One)      `shouldBe` (two, Zero, Zero)    -- zx=1, nx=1, zy=0, ny=1, f=1, no=1 -> y + 1
        alu (one, ones)      (Zero, Zero, One, One, One, Zero)    `shouldBe` (zeros, One, Zero)    -- zx=0, nx=0, zy=1, ny=1, f=1, no=0 -> x - 1
        alu (zeros, ones)    (One, One, Zero, Zero, One, Zero)    `shouldBe` (neg2, Zero, One)    -- zx=1, nx=1, zy=0, ny=0, f=1, no=0 -> y - 1
        alu (one, one)       (Zero, Zero, Zero, Zero, One, Zero)  `shouldBe` (two, Zero, Zero) -- zx=0, nx=0, zy=0, ny=0, f=1, no=0 -> x + y
        alu (one, one)       (Zero, One, Zero, Zero, One, One)    `shouldBe` (zeros, One, Zero)    -- zx=0, nx=1, zy=0, ny=0, f=1, no=1 -> x - y
        alu (num255, ones)   (Zero, Zero, Zero, Zero, Zero, Zero) `shouldBe` (num255, Zero, Zero) -- zx=0, nx=0, zy=0, ny=0, f=0, no=0 -> x AND y
        alu (num255, neg256) (Zero, One, Zero, One, Zero, One)    `shouldBe` (ones, Zero, One)   -- zx=0, nx=1, zy=0, ny=1, f=0, no=1 -> x OR y




