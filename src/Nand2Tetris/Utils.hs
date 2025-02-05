module Nand2Tetris.Utils(
    bus4ToList
    , toBus4
    , bus8ToList
    , toBus8
) where

import BasicPrelude (error)
import Nand2Tetris.Types.Bus

bus4ToList :: Bus4Way a -> [a]
bus4ToList (Bus4Way (x0, x1, x2, x3)) = [x0, x1, x2, x3]

toBus4 :: [a] -> Bus4Way a
toBus4 [x0, x1, x2, x3] = Bus4Way (x0, x1, x2, x3)
toBus4 _ = error "undefined"

bus8ToList :: Bus8Way a -> [a]
bus8ToList (Bus8Way (x0, x1, x2, x3, x4, x5, x6, x7)) = [x0, x1, x2, x3, x4, x5, x6, x7]

toBus8 :: [a] ->  Bus8Way a
toBus8 [x0, x1, x2, x3, x4, x5, x6, x7] = Bus8Way (x0, x1, x2, x3, x4, x5, x6, x7)
toBus8 _ = error "undefined"