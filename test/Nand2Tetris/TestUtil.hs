module Nand2Tetris.TestUtil (
    randomBit
  , random16Bits
) where

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit
import BasicPrelude (IO, replicateM, (<$>))
import System.Random (randomIO)

randomBit :: IO Bit
randomBit = randomIO

random16Bits :: IO HackWord16
random16Bits = toHackWord16 <$> replicateM 16 randomBit

