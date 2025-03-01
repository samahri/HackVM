module Nand2Tetris.Types.Bit(
    Bit(..)
   ,InputBit
   ,OutputBit
   ,Carry
   ,Sum
   ,Sel
   ,Load
   ,Reset
) where

import CorePrelude
import System.Random (Random, randomR, random)
import BasicPrelude(show)

-- TODO learn about Generic typeclass
data Bit = Zero | One deriving (Eq, Enum, Bounded)

type InputBit = Bit
type OutputBit = Bit
type Carry = OutputBit
type Sum = OutputBit

type Reset = Bit
type Sel = Bit
type Load = Bit

instance Show Bit where
    show :: Bit -> String
    show One = "1"
    show Zero = "0"

instance Random Bit where
    -- randomR :: RandomGen g => (Bit, Bit) -> g -> (a, g) 
    randomR (lo, hi) gen = 
        let (n, gen') = randomR (fromEnum lo, fromEnum hi) gen
        in (toEnum n, gen')

    random gen = 
        let (n, gen') = randomR (fromEnum (minBound :: Bit), fromEnum (maxBound :: Bit)) gen
        in (toEnum n, gen')