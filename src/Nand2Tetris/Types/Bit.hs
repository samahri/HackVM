{-# LANGUAGE FlexibleInstances #-}
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
    ( Bounded(..),
      Enum(toEnum, fromEnum),
      Eq,
      Monad(return),
      Show,
      String )
import System.Random (Random, randomR, random)
import BasicPrelude(show, (++))
import Test.QuickCheck ( Arbitrary(arbitrary), chooseAny, Gen )

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

instance Arbitrary Bit where
    arbitrary :: Gen Bit
    arbitrary = chooseAny
    -- arbitrary = elements [Zero, One] -- after Random is removed

instance Arbitrary (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) where
    arbitrary = do
        b0 <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        b3 <- arbitrary

        b4 <- arbitrary
        b5 <- arbitrary
        b6 <- arbitrary
        b7 <- arbitrary

        b8 <- arbitrary
        b9 <- arbitrary
        b10 <- arbitrary
        b11 <- arbitrary

        return (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)

instance Arbitrary (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) where
    arbitrary = do
        b0 <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        b3 <- arbitrary

        b4 <- arbitrary
        b5 <- arbitrary
        b6 <- arbitrary
        b7 <- arbitrary

        b8 <- arbitrary
        b9 <- arbitrary
        b10 <- arbitrary
        b11 <- arbitrary
        b12 <- arbitrary
        b13 <- arbitrary

        return (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)

instance Arbitrary (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) where
    arbitrary = do
        b0 <- arbitrary
        b1 <- arbitrary
        b2 <- arbitrary
        b3 <- arbitrary

        b4 <- arbitrary
        b5 <- arbitrary
        b6 <- arbitrary
        b7 <- arbitrary

        b8 <- arbitrary
        b9 <- arbitrary
        b10 <- arbitrary
        b11 <- arbitrary

        b12 <- arbitrary
        b13 <- arbitrary
        b14 <- arbitrary
        b15 <- arbitrary

        return (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)

instance Show (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) where
    show (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
        "(" ++ show a1 ++ "," ++ show a2 ++ "," ++ show a3 ++ "," ++ show a4 ++ "," ++
        show a5 ++ "," ++ show a6 ++ "," ++ show a7 ++ "," ++ show a8 ++ "," ++
        show a9 ++ "," ++ show a10 ++ "," ++ show a11 ++ "," ++ show a12 ++ "," ++
        show a13 ++ "," ++ show a14 ++ "," ++ show a15 ++ "," ++ show a16 ++ ")"