module Nand2Tetris.TestUtil where

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Utils
import BasicPrelude (IO, Int, replicateM, (<$>), (+), (^), (==), mod, foldr, fst)
import System.Random (randomIO)

randomBit :: IO Bit
randomBit = randomIO

random16Bits :: IO HackWord16
random16Bits = toHackWord16 <$> replicateM 16 randomBit

type Ram8State = Bus8Way HackWord16
randomRam8 :: IO Ram8State
randomRam8 = toBus8 <$> replicateM 8 random16Bits

type Ram64State = Bus8Way Ram8State
randomRam64 :: IO Ram64State
randomRam64 = toBus8 <$> replicateM 8 randomRam8

type Ram512State = Bus8Way Ram64State
randomRam512 :: IO Ram512State
randomRam512 = toBus8 <$> replicateM 8 randomRam64

type Ram4KState = Bus8Way Ram512State
randomRam4K :: IO Ram4KState
randomRam4K = toBus8 <$> replicateM 8 randomRam512

type Ram16KState = Bus4Way Ram4KState
randomRam16K :: IO Ram16KState
randomRam16K = toBus4 <$> replicateM 4 randomRam4K

type ROM32kState = Bus2Way Ram16KState
randomRom32K :: IO ROM32kState
randomRom32K = toBus2 <$> replicateM 2 randomRam16K 


-- TODO: make HackWord16 Foldable
convertToInt :: HackWord16 -> Int
convertToInt input = fst (foldr func (0, 0) (toList input)) `mod` 256
    where
        func :: Bit -> (Int, Int) -> (Int, Int)
        func b (total, acc) = if b == Zero then (total, acc + 1) else (total + 2^acc, acc + 1)
