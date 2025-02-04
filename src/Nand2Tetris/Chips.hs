module Nand2Tetris.Chips (
    halfAdder
   ,fullAdder
   ,add16
   ,inc16
   ,alu
) where

import Nand2Tetris.Gates
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit(Bit(One, Zero))
import BasicPrelude ((==), ($), error)
import Data.List (reverse, replicate)
import Data.List.NonEmpty (head, fromList)

type Input = Bit
type Output = Bit
type Carry = Output
type Sum = Output

-- adds two bits
halfAdder :: (Input, Input) -> (Carry, Sum)
halfAdder (a, b) = (and (a, b), xor (a, b))

-- adds three bits
fullAdder :: (Input, Input, Input)-> (Carry, Sum)
fullAdder (a, b, c) = (carry, sum)
    where
        sum = mux (xor (b, c), not $ xor (b, c)) a
        carry = mux (and (b, c), or (b, c)) a

type Input16 = HackWord16
type Output16 = HackWord16

add16 :: (Input16, Input16) -> Output16 
add16 (a16, b16) = toHackWord16 $ reverse (go (reverse a16t) (reverse b16t) Zero)
    where
        a16t = toList a16
        b16t = toList b16
        go :: [Input] -> [Input] -> Carry -> [Output]
        go [] [] _ = []
        go [] (_:_) _= error "impossible state"
        go (_:_) [] _ = error "impossible state"
        go (a:as) (b:bs) c = let (carry, sum) = fullAdder (a, b, c) in sum : go as bs carry 

inc16 :: Input16 -> Output16
inc16 in16 = toHackWord16 $ reverse (go (reverse in16t) One)
    where
        in16t = toList in16

        go :: [Input] -> Carry -> [Output]
        go [] _ = []
        go (a:as) c = let (carry, sum) = halfAdder (a, c) in sum : go as carry

type Zx = Bit
type Nx = Bit
type Zy = Bit
type Ny = Bit
type F = Bit
type No = Bit
type AluCtrl = (Zx, Nx, Zy, Ny, F, No)

type Zr = Bit
type Ng = Bit

alu :: (Input16, Input16) -> AluCtrl -> (Output16, Zr, Ng)
alu (x16, y16) (zx, nx, zy, ny, f, no) = (out, zr, ng)
    where
        zeroBits :: Output16
        zeroBits = toHackWord16 $ replicate 16 Zero

        out :: Output16
        out = mux4Way16 (and16 (x,y), add16 (x,y), not16 $ and16 (x,y), not16 $ add16 (x,y)) (no, f)
            where
                x :: HackWord16
                x = mux4Way16 (x16, zeroBits, not16 x16, not16 zeroBits) (nx, zx)

                y :: HackWord16
                y = mux4Way16 (y16, zeroBits, not16 y16, not16 zeroBits) (ny, zy)
        
        zr :: Zr
        zr = if out == zeroBits then One else Zero
        
        ng :: Ng
        ng = head (fromList (toList out))
