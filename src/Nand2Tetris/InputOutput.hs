module Nand2Tetris.InputOutput(
    screen
    , ScreenAddress
    , ScreenState
    , keyboard
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Memory
import Nand2Tetris.Gates

import BasicPrelude (IO, pure, (.))
import Control.Monad.Trans.State.Strict (State, get, put)

type Input16 = HackWord16
type Output16 = HackWord16

type Load = Bit

type ScreenAddress = (Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit) -- 13 bit address; first bit is ignored
type ScreenState = Bus4Way Ram4kState
type ScreenOutput = State ScreenState Output16

screen :: ScreenAddress -> Input16 -> Load -> ScreenOutput
screen (_, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13) input16 load = do
    screenState <- get
    let inputBus = dMux4Way16 input16 ram4KSelector
        loadArr = dMux4Way load ram4KSelector

        memroyFunction = ram4K ram4KMemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr screenState
        
        screenOutput = mux4WayRam registerOutput ram4KSelector

    put nextCycleOutput
    pure screenOutput
    where
        ram4KMemoryBus = (Zero, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11)
        ram4KSelector = (sel12, sel13)

type KeyboardOutput = HackWord16

keyboard :: IO KeyboardOutput
keyboard = (pure . pure) Zero -- no output