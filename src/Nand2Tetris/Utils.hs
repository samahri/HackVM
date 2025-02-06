module Nand2Tetris.Utils(
    toBus4
    , toBus8
    , combine4Way
    -- , commbine3w3
) where

import BasicPrelude (error)
import Nand2Tetris.Types.Bus

toBus4 :: [a] -> Bus4Way a
toBus4 [x0, x1, x2, x3] = Bus4Way (x0, x1, x2, x3)
toBus4 _ = error "undefined"

toBus8 :: [a] ->  Bus8Way a
toBus8 [x0, x1, x2, x3, x4, x5, x6, x7] = Bus8Way (x0, x1, x2, x3, x4, x5, x6, x7)
toBus8 _ = error "undefined"

combine4Way :: Bus4Way a -> Bus4Way a -> Bus8Way a
combine4Way (Bus4Way (b0, b1, b2, b3)) (Bus4Way (b4, b5, b6, b7)) = Bus8Way (b0, b1, b2, b3, b4, b5, b6, b7)