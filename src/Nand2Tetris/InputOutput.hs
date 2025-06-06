{-# LANGUAGE LambdaCase #-}
module Nand2Tetris.InputOutput(
    screen
    , ScreenAddress
    , ScreenState
    , KeyboardIO
    , keyboard
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.Bus
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Memory(RAM4kState)
import Nand2Tetris.Memory
import Nand2Tetris.Gates

import BasicPrelude (IO, pure, (.), Char, (<$>), ($), liftIO)
import Control.Monad.Trans.State.Strict (State, get, put)
import System.IO (stdin, hReady, getChar)

type ScreenAddress = (Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit) -- 13 bit address; first bit is ignored
type ScreenState = Bus4Way RAM4kState
type ScreenOutput = Output16
type Screen = State ScreenState ScreenOutput

screen :: ScreenAddress -> Input16 -> Load -> Screen
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
type KeyboardIO = IO

keyboard :: KeyboardIO KeyboardOutput
keyboard = do
    buttonPressed <- liftIO $ hReady stdin
    if buttonPressed 
        then getScanCode <$> getChar
        else (pure . pure) Zero

getScanCode :: Char -> KeyboardOutput
getScanCode = \case
    '\ESC' -> HackWord16F (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One, Zero, Zero, Zero, One, One, Zero, Zero) -- 140
    'j' -> HackWord16F (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One, One, Zero, One, Zero, One, Zero) -- 106
    _ -> pure Zero