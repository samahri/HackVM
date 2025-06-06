module Nand2Tetris.Chips (
    AluCtrl(..)
   ,halfAdder
   ,fullAdder
   ,add16
   ,inc16
   ,alu
) where

import Nand2Tetris.Gates
    ( not, and, or, xor, mux, not16, and16, mux4Way16 )
import Nand2Tetris.Types.HackWord16
    ( Output16, Input16, HackWord16, toHackWord16, toList )
import Nand2Tetris.Types.Bit
    ( Sum, Carry, OutputBit, InputBit, Bit(..) )
import Nand2Tetris.Types.Bus ( Bus4Way(Bus4Way) )
import BasicPrelude ((==), ($), error, Show, Eq)
import Data.List (reverse, replicate)
import Data.List.NonEmpty (head, fromList)

type Zx = Bit
type Nx = Bit
type Zy = Bit
type Ny = Bit
type F = Bit
type No = Bit

type Zr = Bit
type Ng = Bit

data AluCtrl = AluCtrl {zx :: Zx, nx :: Nx, zy :: Zy, ny :: Ny, f :: F, no :: No} deriving (Show, Eq)

-- adds two bits
halfAdder :: (InputBit, InputBit) -> (Carry, Sum)
halfAdder (a, b) = (and (a, b), xor (a, b))

-- adds three bits
fullAdder :: (InputBit, InputBit, InputBit)-> (Carry, Sum)
fullAdder (a, b, c) = (carry, sum)
    where
        sum = mux (xor (b, c), not $ xor (b, c)) a
        carry = mux (and (b, c), or (b, c)) a

add16 :: (Input16, Input16) -> Output16 
add16 (a16, b16) = toHackWord16 $ reverse (go (reverse a16t) (reverse b16t) Zero)
    where
        a16t = toList a16
        b16t = toList b16
        go :: [InputBit] -> [InputBit] -> Carry -> [OutputBit]
        go [] [] _ = []
        go [] (_:_) _= error "impossible state"
        go (_:_) [] _ = error "impossible state"
        go (a:as) (b:bs) c = let (carry, sum) = fullAdder (a, b, c) in sum : go as bs carry 

inc16 :: Input16 -> Output16
inc16 in16 = toHackWord16 $ reverse (go (reverse in16t) One)
    where
        in16t = toList in16

        go :: [InputBit] -> Carry -> [OutputBit]
        go [] _ = []
        go (a:as) c = let (carry, sum) = halfAdder (a, c) in sum : go as carry

alu :: (Input16, Input16) -> AluCtrl -> (Output16, Zr, Ng)
alu (x16, y16) aluCtrl = (out, zr, ng)
    where
        zeroBits :: Output16
        zeroBits = toHackWord16 $ replicate 16 Zero

        out :: Output16
        out = mux4Way16 (Bus4Way (and16 (x,y), add16 (x,y), not16 $ and16 (x,y), not16 $ add16 (x,y))) (no aluCtrl, f aluCtrl)
            where
                x :: HackWord16
                x = mux4Way16 (Bus4Way (x16, zeroBits, not16 x16, not16 zeroBits)) (nx aluCtrl, zx aluCtrl)

                y :: HackWord16
                y = mux4Way16 (Bus4Way (y16, zeroBits, not16 y16, not16 zeroBits)) (ny aluCtrl, zy aluCtrl)
        
        zr :: Zr
        zr = if out == zeroBits then One else Zero
        
        ng :: Ng
        ng = head (fromList (toList out))
