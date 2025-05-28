module Nand2Tetris.TestUtil where

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Memory
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Chips
import Nand2Tetris.Utils
import BasicPrelude
import System.Random (randomIO, randomRIO)
import Test.QuickCheck

genAInstruction :: Gen HackWord16
genAInstruction = do
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

    pure $ HackWord16F (Zero, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)

randomBit :: IO Bit
randomBit = randomIO

random16Bits :: IO HackWord16
random16Bits = toHackWord16 <$> replicateM 16 randomBit

random3BitAddress :: IO (Bit, Bit, Bit)
random3BitAddress = do
    bit1 <- randomBit
    bit2 <- randomBit
    bit3 <- randomBit
    pure (bit1, bit2, bit3)

one :: HackWord16
one = toHackWord16 $ replicate 15 Zero <> [One] -- 0000 0000 0000 0001

zeros :: HackWord16
zeros = toHackWord16 $ replicate 16 Zero

randomRam8 :: IO RAM8State
randomRam8 = toBus8 <$> replicateM 8 random16Bits

randomRam64 :: IO RAM64State
randomRam64 = toBus8 <$> replicateM 8 randomRam8

randomRam512 :: IO RAM512State
randomRam512 = toBus8 <$> replicateM 8 randomRam64

randomRam4K :: IO RAM4kState
randomRam4K = toBus8 <$> replicateM 8 randomRam512

type RAM8kState = Bus2Way RAM4kState
randomRam8K :: IO RAM8kState
randomRam8K = toBus2 <$> replicateM 2 randomRam4K

randomRam16K :: IO RAM16kState
randomRam16K = toBus4 <$> replicateM 4 randomRam4K

random32KMemory :: IO ROM32kState
random32KMemory = toBus2 <$> replicateM 2 randomRam16K 

genRandomAluCtrl :: Gen (Bit, AluCtrl)
genRandomAluCtrl = elements aluCtrls

getRandomAluCtrl :: IO (Bit, AluCtrl)
getRandomAluCtrl = do
    index <- randomRIO (0, length aluCtrls - 1)
    pure (aluCtrls !! index)

aluCtrls :: [(Bit, AluCtrl)]
aluCtrls =
  [ (Zero, AluCtrl { zx = One,  nx = Zero, zy = One,  ny = Zero, f = One,  no = Zero })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = One,  ny = One,  f = One,  no = One  })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = One,  ny = Zero, f = One,  no = Zero })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = One,  ny = One,  f = Zero, no = Zero }) 
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = Zero, no = Zero })
  , (One,  AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = Zero, no = Zero }) 
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = One,  ny = One,  f = Zero, no = One  })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = Zero, no = One  })
  , (One,  AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = Zero, no = One  })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = One,  ny = One,  f = One,  no = One  })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = One,  no = One  })
  , (One,  AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = One,  no = One  })
  , (Zero, AluCtrl { zx = Zero, nx = One,  zy = One,  ny = One,  f = One,  no = One  })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = Zero, ny = One,  f = One,  no = One  })
  , (One,  AluCtrl { zx = One,  nx = One,  zy = Zero, ny = One,  f = One,  no = One  })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = One,  ny = One,  f = One,  no = Zero })
  , (Zero, AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = One,  no = Zero })
  , (One,  AluCtrl { zx = One,  nx = One,  zy = Zero, ny = Zero, f = One,  no = Zero })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = Zero, f = One,  no = Zero })
  , (One,  AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = Zero, f = One,  no = Zero })
  , (Zero, AluCtrl { zx = Zero, nx = One,  zy = Zero, ny = Zero, f = One,  no = One  })
  , (One,  AluCtrl { zx = Zero, nx = One,  zy = Zero, ny = Zero, f = One,  no = One  })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = One,  f = One,  no = One  })
  , (One,  AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = One,  f = One,  no = One  })
  , (Zero, AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = Zero, f = Zero, no = Zero })
  , (One,  AluCtrl { zx = Zero, nx = Zero, zy = Zero, ny = Zero, f = Zero, no = Zero })
  , (Zero, AluCtrl { zx = Zero, nx = One,  zy = Zero, ny = One,  f = Zero, no = One  })
  , (One,  AluCtrl { zx = Zero, nx = One,  zy = Zero, ny = One,  f = Zero, no = One  })
  ]