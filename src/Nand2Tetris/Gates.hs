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
  , mux8WayRam
  , mux4WayRam
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bus
import Nand2Tetris.Utils
import BasicPrelude ((.), (==), foldr1, (<$>))
import Control.Applicative (pure, liftA2)
import Data.Tuple (curry)

type Input16 = HackWord16
type Output16 = HackWord16

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

dmux16 :: Input16 -> Sel -> (Output16, Output16)
dmux16 input sel = case sel of
    Zero -> (input, zeros)
    One -> (zeros, input)

not16 :: Input16 -> Output16
not16 input = not <$> input
    
and16 :: (Input16, Input16) -> Output16
and16 (input1, input2) = liftA2 (curry and) input1 input2

or16 :: (Input16, Input16) -> Output16
or16 (input1, input2) = liftA2 (curry or) input1 input2

-- TODO use logic gates
mux16 :: (Input16, Input16) -> Sel -> Output16
mux16 (input1, input2) sel = case sel of
    Zero -> input1
    One -> input2

or8Way :: Bus4Way Input -> Input
or8Way input = let inputArr = bus4ToList input in foldr1 (curry or) inputArr
    where
        -- TODO: implement foldable
        bus4ToList :: Bus4Way a -> [a]
        bus4ToList (Bus4Way (x0, x1, x2, x3)) = [x0, x1, x2, x3]
        
-- TODO use logic gates
mux4Way16 :: Bus4Way Input16 -> (Sel, Sel) -> Output16 
mux4Way16 (Bus4Way (a, b, c, d)) sel = case sel of
    (Zero, Zero) -> a
    (Zero, One) -> b
    (One, Zero) -> c
    (One, One) -> d

-- TODO use logic gates
dMux4Way :: Input -> (Sel, Sel) -> Bus4Way Output
dMux4Way input sel = case sel of
    (Zero, Zero) -> Bus4Way (input, Zero, Zero, Zero)
    (Zero, One) -> Bus4Way (Zero, input, Zero, Zero)
    (One, Zero) -> Bus4Way (Zero, Zero, input, Zero)
    (One, One) -> Bus4Way (Zero, Zero, Zero, input)

dMux8Way :: Input -> (Sel, Sel, Sel) -> Bus8Way Output
dMux8Way input (sel0, sel1, sel2) = combine4Way upper lower
    where
        lower = if sel0 == Zero then pure Zero else dMux4Way input (sel1, sel2) 
        upper = if sel0 == One then pure Zero else dMux4Way input (sel1, sel2)

dMux4Way16 :: Input16 -> (Sel, Sel) -> Bus4Way Output16
dMux4Way16 input sel = case sel of
    (Zero, Zero) -> Bus4Way (input, zeros, zeros, zeros)
    (Zero, One) -> Bus4Way (zeros, input, zeros, zeros)
    (One, Zero) -> Bus4Way (zeros, zeros, input, zeros) 
    (One, One) -> Bus4Way (zeros, zeros, zeros, input)

mux8Way16 :: Bus8Way Input16 -> (Sel, Sel, Sel) -> Output16
mux8Way16 inputBus (addr0, addr1, addr2) = mux16 (mux4Way16 registers7to4 addressTuple, mux4Way16 registers3to0 addressTuple) addr0
    where
        addressTuple = (addr1, addr2)
        (registers7to4, registers3to0) = split8Bus inputBus

dMux8Way16 :: Input16 -> (Sel, Sel, Sel) -> Bus8Way Output16
dMux8Way16 input16 (sel0, sel1, sel2) = combine4Way upper lower
    where
        lower = if sel0 == One then dMux4Way16 input16 (sel1, sel2) else  pure zeros
        upper = if sel0 == Zero then dMux4Way16 input16 (sel1, sel2) else pure zeros

mux8WayRam :: Bus8Way a -> (Sel, Sel, Sel) -> a
mux8WayRam (Bus8Way(ram0, ram1, ram2, ram3, ram4, ram5, ram6, ram7)) sel = case sel of
    (Zero, Zero, Zero) -> ram0
    (Zero, Zero, One) -> ram1
    (Zero, One, Zero) -> ram2
    (Zero, One, One) -> ram3
    (One, Zero, Zero) -> ram4
    (One, Zero, One) -> ram5
    (One, One, Zero) -> ram6
    (One, One, One)  -> ram7

mux4WayRam :: Bus4Way a -> (Sel, Sel) -> a
mux4WayRam (Bus4Way (a, b, c, d)) sel = case sel of
    (Zero, Zero) -> a
    (Zero, One) -> b
    (One, Zero) -> c
    (One, One) -> d

split8Bus :: Bus8Way a -> (Bus4Way a, Bus4Way a)
split8Bus (Bus8Way (x1, x2, x3, x4, x5, x6, x7, x8)) = (Bus4Way (x1, x2, x3, x4), Bus4Way (x5, x6, x7, x8))

zeros :: HackWord16
zeros = pure Zero