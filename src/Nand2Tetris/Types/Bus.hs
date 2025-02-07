{-# LANGUAGE DeriveFunctor #-}
module Nand2Tetris.Types.Bus (
    Bus2Way(..)
   ,Bus4Way(..)
   ,Bus8Way(..)
) where

import BasicPrelude (Eq, Show, show, (++), Functor, )
import Control.Applicative (Applicative, pure, (<*>))

newtype Bus2Way a = Bus2Way (a, a) deriving (Eq, Functor)

newtype Bus4Way a = Bus4Way (a, a, a, a) deriving (Eq, Functor)

newtype Bus8Way a = Bus8Way (a, a, a, a, a, a, a, a) deriving (Eq, Functor)

instance (Show a) => Show (Bus2Way a) where
  show (Bus2Way (a, b)) = "Bus2Way (" ++ show a ++ ", " ++ show b ++ ")"

instance (Show a) => Show (Bus4Way a) where
  show (Bus4Way (a, b, c, d)) = "Bus4Way (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ")"

instance (Show a) => Show (Bus8Way a) where
  show (Bus8Way (a, b, c, d, e, f, g, h)) = "Bus8Way (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ ", " ++ show f ++ ", " ++ show g ++ ", " ++ show h ++ ")"

instance Applicative Bus2Way where
  pure :: a -> Bus2Way a
  pure x = Bus2Way (x, x)

  (<*>) :: Bus2Way (a -> b) -> Bus2Way a -> Bus2Way b
  (Bus2Way (fn1, fn2)) <*> (Bus2Way (x1, x2)) = Bus2Way (fn1 x1, fn2 x2)

instance Applicative Bus4Way where
  pure :: a -> Bus4Way a
  pure x = Bus4Way (x, x, x, x)

  (<*>) :: Bus4Way (a -> b) -> Bus4Way a -> Bus4Way b
  (Bus4Way (fn1, fn2, fn3, fn4)) <*> (Bus4Way (x1, x2, x3, x4)) = Bus4Way (fn1 x1, fn2 x2, fn3 x3, fn4 x4)

instance Applicative Bus8Way where
  pure :: a -> Bus8Way a
  pure x = Bus8Way (x, x, x, x, x, x, x, x)

  (<*>) :: Bus8Way (a -> b) -> Bus8Way a -> Bus8Way b
  (Bus8Way (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8)) <*> (Bus8Way (x1, x2, x3, x4, x5, x6, x7, x8)) 
    = Bus8Way (fn1 x1, fn2 x2, fn3 x3, fn4 x4, fn5 x5, fn6 x6, fn7 x7, fn8 x8)