{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Nand2Tetris.Types.HackWord16 (
    toHackWord16 
   , toList
   , HackWord16
   , HackWord16F(..)
) where

import Nand2Tetris.Types.Bit(Bit)
import Data.List (length)
import BasicPrelude ((==), ($), Eq, (&&), Show, show, (++), error, Functor)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Exception (assert)

newtype HackWord16F a = HackWord16F (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) deriving (Functor)

type HackWord16 = HackWord16F Bit

-- use isos
toHackWord16 :: [Bit] -> HackWord16
toHackWord16 list = assert (length list == 16) $ converttoHackWord16 list
    where
        converttoHackWord16 :: [Bit] -> HackWord16
        converttoHackWord16 [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15] = HackWord16F (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)
        converttoHackWord16 _ = error "undefined"

toList :: HackWord16 -> [Bit]
toList (HackWord16F (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)) = [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15]

instance Applicative HackWord16F where
  pure :: a -> HackWord16F a
  pure x = HackWord16F (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)

  (<*>) :: HackWord16F (a -> b) -> HackWord16F a -> HackWord16F b
  (HackWord16F (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16)) <*> (HackWord16F (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)) 
    = HackWord16F (fn1 x1, fn2 x2, fn3 x3, fn4 x4, fn5 x5, fn6 x6, fn7 x7, fn8 x8, fn9 x9, fn10 x10, fn11 x11, fn12 x12, fn13 x13, fn14 x14, fn15 x15, fn16 x16)

instance Show HackWord16 where
    show (HackWord16F (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)) =
        "(" 
        ++ show x1 ++ ", " ++ show x2 ++ ", " ++ show x3 ++ ", " ++ show x4 ++ ", " ++ show x5 ++ ", " ++
        show x6 ++ ", " ++ show x7 ++ ", " ++ show x8 ++ ", " ++ show x9 ++ ", " ++ show x10 ++ ", " ++
        show x11 ++ ", " ++ show x12 ++ ", " ++ show x13 ++ ", " ++ show x14 ++ ", " ++ show x15 ++ ", " ++
        show x16 ++ ")"

instance Eq HackWord16 where
    HackWord16F (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) == HackWord16F (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15)
        =
        (x0 == y0) &&
        (x1 == y1) &&
        (x2 == y2) &&
        (x3 == y3) &&
        (x4 == y4) &&
        (x5 == y5) &&
        (x6 == y6) &&
        (x7 == y7) &&
        (x8 == y8) &&
        (x9 == y9) &&
        (x10 == y10) &&
        (x11 == y11) &&
        (x12 == y12) &&
        (x13 == y13) &&
        (x14 == y14) &&
        (x15 == y15)