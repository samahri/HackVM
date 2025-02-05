module Nand2Tetris.Gates(
    nand
  , not
  , not16
  , and
  , and16
  , or
  , or16
  , nor
  , xor
  , mux
  , mux16
  , dmux
  , dmux16
  , or8Way
  , mux4Way16
  , mux8Way16
  , dMux4Way
  , dMux4Way16
  , dMux8Way
  , dMux8Way16
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16
import BasicPrelude ((.), zipWith, length, (==), foldr1, (<$>), ($), error)
import Control.Exception (assert)
import Data.List (replicate, splitAt)
import Data.Tuple (curry)

type Input = Bit
type Output = Bit
type Sel = Bit

nand :: (Input, Input) -> Output
nand (One, One) = Zero
nand _ = One

not :: Input -> Output
not a = nand (a, a)

and :: (Input, Input) -> Output
and = not . nand

or :: (Input, Input) -> Output
or (a, b) = nand (not a, not b)

nor :: (Input, Input) -> Output
nor = not . or

xor :: (Input, Input) -> Output
xor (a, b) = nand (nand (a, r1), nand (r1, b))
    where
        r1 = nand (a, b)

mux :: (Input, Input) -> Sel -> Output
mux (a , b) sel = or (o1, o2)
    where
        o1 = and (a, not sel)
        o2 = and (b, sel)

dmux :: Input -> Sel -> (Output, Output)
dmux input sel = (o1, o2)
    where
        o1 = and (input, not sel)
        o2 = and (input , sel)

type Input16 = HackWord16
type Output16 = HackWord16

dmux16 :: Input16 -> Sel -> (Output16, Output16)
dmux16 input sel = case sel of
    Zero -> (input, zeros)
    One -> (zeros, input)

not16 :: Input16 -> Output16
not16 input = toHackWord16 (not <$> inputArr)
    where
        inputArr = toList input
    
and16 :: (Input16, Input16) -> Output16
and16 (input1, input2) = toHackWord16 (zipWith (curry and) inputArr1 inputArr2) 
    where
        inputArr1 = toList input1
        inputArr2 = toList input2

or16 :: (Input16, Input16) -> Output16
or16 (input1, input2) = toHackWord16 (zipWith (curry or) inputArr1 inputArr2) 
    where
        inputArr1 = toList input1
        inputArr2 = toList input2

-- use logic gates
mux16 :: (Input16, Input16) -> Sel -> Output16
mux16 (input1, input2) sel = case sel of
    Zero -> input1
    One -> input2

-- TODO: replace array with Bus8
or8Way :: [Bit] -> Output
or8Way inputArr = assert (length inputArr == 8) (foldr1 (curry or) inputArr)

-- use logic gates
mux4Way16 :: (Input16, Input16, Input16, Input16) -> (Sel, Sel) -> Output16
mux4Way16 (a, b, c, d) sel = case sel of
    (Zero, Zero) -> a
    (Zero, One) -> b
    (One, Zero) -> c
    (One, One) -> d

-- use logic gates
dMux4Way :: Input -> (Sel, Sel) -> (Output, Output, Output, Output)
dMux4Way input sel = case sel of
    (Zero, Zero) -> (input, Zero, Zero, Zero)
    (Zero, One) -> (Zero, input, Zero, Zero)
    (One, Zero) -> (Zero, Zero, input, Zero) 
    (One, One) -> (Zero, Zero, Zero, input)

-- TODO: replace it with newtype
type Bus8Way = (Output, Output, Output, Output, Output, Output, Output, Output)  

dMux8Way :: Input -> (Sel, Sel, Sel) -> Bus8Way 
dMux8Way input (sel0, sel1, sel2) = combine (upper, lower)
    where
        lower = if sel0 == Zero then (Zero, Zero, Zero, Zero) else dMux4Way input (sel1, sel2) 
        upper = if sel0 == One then (Zero, Zero, Zero, Zero) else dMux4Way input (sel1, sel2)

dMux4Way16 :: Input16 -> (Sel, Sel) -> (Output16, Output16, Output16, Output16)
dMux4Way16 input sel = case sel of
    (Zero, Zero) -> (input, zeros, zeros, zeros)
    (Zero, One) -> (zeros, input, zeros, zeros)
    (One, Zero) -> (zeros, zeros, input, zeros) 
    (One, One) -> (zeros, zeros, zeros, input)

type Bus8Way16 = (Output16, Output16, Output16, Output16, Output16, Output16, Output16, Output16) 

mux8Way16 :: [Input16] -> (Sel, Sel, Sel) -> Output16
mux8Way16 registerList (addr0, addr1, addr2) = assert (length registerList == 8) $ mux16 (mux4Way16 registers7to4 addressTuple, mux4Way16 registers3to0 addressTuple) addr0
    where
        addressTuple = (addr1, addr2)
        (registers7to4, registers3to0) = let (r7to4l, r3to0l) = splitAt 4 registerList in (to4Tuple r7to4l, to4Tuple r3to0l)

        to4Tuple :: [Input16] -> (Input16, Input16, Input16, Input16)
        to4Tuple [b0, b1, b2, b3] = (b0, b1, b2, b3)
        to4Tuple _ = error "undefined"

dMux8Way16 :: Input16 -> (Sel, Sel, Sel) -> Bus8Way16
dMux8Way16 input16 (sel0, sel1, sel2) = combine (upper, lower)
    where
        lower = if sel0 == One then dMux4Way16 input16 (sel1, sel2) else  (zeros, zeros, zeros, zeros)
        upper = if sel0 == Zero then dMux4Way16 input16 (sel1, sel2) else (zeros, zeros, zeros, zeros)

combine :: ((a, a, a, a), (a, a, a, a)) -> (a, a, a, a, a, a, a, a)
combine ((b0, b1, b2, b3), (b4, b5, b6, b7)) = (b0, b1, b2, b3, b4, b5, b6, b7)

zeros :: HackWord16
zeros = toHackWord16 $ replicate 16 Zero
