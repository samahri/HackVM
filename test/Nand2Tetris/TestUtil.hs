module Nand2Tetris.TestUtil where

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Chips
import Nand2Tetris.Utils
import BasicPrelude
import System.Random (randomIO, randomRIO)

randomBit :: IO Bit
randomBit = randomIO

random16Bits :: IO HackWord16
random16Bits = toHackWord16 <$> replicateM 16 randomBit

randomAInstruction :: IO HackWord16
randomAInstruction = do
    number <- replicateM 15 randomBit
    let output = toHackWord16 $ [Zero] <> number
    pure output

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

randomRam8K :: IO (Bus2Way Ram4KState)
randomRam8K = toBus2 <$> replicateM 2 randomRam4K

type Ram16KState = Bus4Way Ram4KState
randomRam16K :: IO Ram16KState
randomRam16K = toBus4 <$> replicateM 4 randomRam4K

type ROM32kState = Bus2Way Ram16KState
random32KMemory :: IO ROM32kState
random32KMemory = toBus2 <$> replicateM 2 randomRam16K 


getRandomAluCtrl :: IO (Bit, AluCtrl)
getRandomAluCtrl = do
    index <- randomRIO (0, length aluCtrls - 1)
    pure (aluCtrls !! index)

aluCtrls :: [(Bit, AluCtrl)]
aluCtrls = [  (Zero, AluCtrl {zx = One, nx = Zero, zy = One, ny = Zero, f = One, no = Zero}),
              (Zero, AluCtrl {zx = One, nx = One,  zy =One,  ny = One, f = One, no = One}),
              (Zero, AluCtrl {zx = One, nx = One,  zy =One, ny = Zero, f = One, no = Zero}),
              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = Zero, no = Zero}),

              (Zero, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = Zero}),
              (One , AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = Zero}),  

              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = Zero,no = One})  ,  

              (Zero, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = One}) ,  
              (One, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = Zero, no = One}) ,  

              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = One, no = One})   , 

              (Zero, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = One})   , 
              (One, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = One})   , 

              (Zero, AluCtrl {zx = Zero, nx = One,  zy =One,  ny =One,  f = One, no = One})   , 

              (Zero, AluCtrl {zx = One, nx = One,  zy =Zero, ny = One,  f = One, no = One})   , 
              (One, AluCtrl {zx = One, nx = One,  zy =Zero, ny = One,  f = One, no = One})   , 

              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = One, ny = One, f = One, no = Zero})  , 

              (Zero, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = Zero})  , 
              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  One, no = Zero}), 
              (Zero, AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = Zero, f = One, no = One})  , 
              (Zero, AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  Zero, no = Zero}),
              (Zero, AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = One, f = Zero,no = One}),    
              
              (One, AluCtrl {zx = One, nx = One,  zy =Zero, ny = Zero, f = One, no = Zero})  , 
              (One, AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  One, no = Zero}), 
              (One, AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = Zero, f = One, no = One})  , 
              (One, AluCtrl {zx = Zero, nx = Zero, zy = Zero, ny = Zero,f =  Zero, no = Zero}),
              (One, AluCtrl {zx = Zero, nx = One,  zy =Zero, ny = One, f = Zero,no = One})    
            ]