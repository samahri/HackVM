module Nand2Tetris.Types.Bus (
    Bus4Way(..)
   ,Bus8Way(..)
) where

import BasicPrelude (Eq, Show, show, (++))

newtype Bus4Way a = Bus4Way (a, a, a, a) deriving (Eq)

newtype Bus8Way a = Bus8Way (a, a, a, a, a, a, a, a) deriving (Eq)

instance (Show a) => Show (Bus4Way a) where
  show (Bus4Way (a, b, c, d)) = "Bus4Way (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ")"

instance (Show a) => Show (Bus8Way a) where
  show (Bus8Way (a, b, c, d, e, f, g, h)) = "Bus8Way (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", " ++ show e ++ ", " ++ show f ++ ", " ++ show g ++ ", " ++ show h ++ ")"
