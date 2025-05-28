module Nand2Tetris.ChipsSpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16 ( toHackWord16, toList )
import Nand2Tetris.TestUtil ( one, zeros )
import BasicPrelude
    ( ($), Eq((==)), Applicative(pure), (<>), head, replicate ) 
import Nand2Tetris.Chips
    ( add16,
      alu,
      fullAdder,
      halfAdder,
      inc16,
      AluCtrl(no, AluCtrl, zx, nx, zy, ny, f) )
import Test.Hspec ( context, specify, shouldBe, Spec )
import Test.QuickCheck ( (===), Testable(property) )

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

    specify "ALU" $
        property $ \x y -> 
            let
                zr = if x == zeros then One else Zero
                ng = head (toList x)
            in
                alu (x, y)  (AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = Zero, no = Zero}) === (x, zr, ng)

    
    specify "ALU" $ do
        -- a = 0; c = 101010
        alu (zeros, ones) (AluCtrl {zx = One, nx = Zero, zy = One, ny = Zero, f = One, no = Zero})     `shouldBe` (zeros, One, Zero)           

        -- zx=1, nx=1, zy=1, ny=1, f=1, no=1 -> 1
        alu (zeros, ones) (AluCtrl {zx = One, nx = One,  zy =One,  ny = One, f = One, no = One})      `shouldBe` (one, Zero, Zero)       

        --       
        alu (num255, num255) (AluCtrl {zx = One, nx = One,  zy =One, ny = Zero, f = One, no =Zero})   `shouldBe` (ones, Zero, One)        
        
        
        -- 
        alu (zeros, num255)  (AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = Zero})   `shouldBe` (num255, Zero, Zero)  -- zx=1, nx=1, zy=0, ny=0, f=0, no=0 -> y
        alu (one, zeros)     (AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = Zero,no = One})    `shouldBe` (neg2, Zero, One)  -- zx=0, nx=0, zy=1, ny=1, f=0, no=1 -> NOT x
        alu (zeros, num255)  (AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = One})    `shouldBe` (neg256, Zero, One)  -- zx=1, nx=1, zy=0, ny=0, f=0, no=1 -> NOT y
        alu (one, zeros)     (AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = One, no = One})    `shouldBe` (ones, Zero, One)     -- zx=0, nx=0, zy=1, ny=1, f=1, no=1 -> -x
        alu (zeros, one)     (AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = One})     `shouldBe` (ones, Zero, One)     -- zx=1, nx=1, zy=0, ny=0, f=1, no=1 -> -y
        alu (zeros, zeros)   (AluCtrl {zx = Zero, nx = One,  zy =One,  ny =One,  f = One, no = One})    `shouldBe` (one, Zero, Zero)    -- zx=0, nx=1, zy=1, ny=1, f=1, no=1 -> x + 1
        alu (zeros, one)     (AluCtrl {zx = One, nx = One,  zy =Zero, ny = One,  f = One, no = One})     `shouldBe` (two, Zero, Zero)    -- zx=1, nx=1, zy=0, ny=1, f=1, no=1 -> y + 1
        alu (one, ones)      (AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = One, no = Zero})    `shouldBe` (zeros, One, Zero)    -- zx=0, nx=0, zy=1, ny=1, f=1, no=0 -> x - 1
        alu (zeros, ones)    (AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = Zero})    `shouldBe` (neg2, Zero, One)    -- zx=1, nx=1, zy=0, ny=0, f=1, no=0 -> y - 1
        alu (one, one)       (AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  One, no = Zero})  `shouldBe` (two, Zero, Zero) -- zx=0, nx=0, zy=0, ny=0, f=1, no=0 -> x + y
        alu (one, one)       (AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = Zero, f = One, no = One})    `shouldBe` (zeros, One, Zero)    -- zx=0, nx=1, zy=0, ny=0, f=1, no=1 -> x - y
        alu (num255, ones)   (AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  Zero, no = Zero}) `shouldBe` (num255, Zero, Zero) --  x AND y
        alu (num255, neg256) (AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = One, f = Zero,no = One})    `shouldBe` (ones, Zero, One)   -- x OR y




