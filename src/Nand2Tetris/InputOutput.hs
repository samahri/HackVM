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

import BasicPrelude (Int, IO, pure, (.), Char, (<$>), ($), liftIO, (<=), (>=), (&&))
import Control.Monad.Trans.State.Strict (State)
import Data.Bits (testBit)
import Data.Char (ord)
import System.IO (stdin, hReady, getChar)

type ScreenAddress = (Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit, Bit, Bit,  Bit, Bit) -- 13 bit address; first bit is ignored
type ScreenState = Bus4Way RAM4kState
type ScreenOutput = Output16
type Screen = State ScreenState ScreenOutput

screen :: ScreenAddress -> Input16 -> Load -> Screen
screen (_, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13) = 
  let
    ram4KMemoryBus = (Zero, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11)
    ram4KSelector = (sel12, sel13)
  in
      memoryBank 
        dMux4Way 
        mux4WayRam
        (ram4K ram4KMemoryBus)
        ram4KSelector

type KeyboardOutput = HackWord16
type KeyboardIO = IO

keyboard :: KeyboardIO KeyboardOutput
keyboard = do
    buttonPressed <- liftIO $ hReady stdin
    if buttonPressed 
        then getScanCode <$> getChar
        else (pure . pure) Zero

getScanCode :: Char -> KeyboardOutput
getScanCode = toKeyboardOutput . \case
    '\ESC' -> 140  -- Escape
    '\n'   -> 128  -- Enter: line feed
    '\r'   -> 128  -- Enter: carriage return
    '\BS'  -> 129  -- Backspace
    '\DEL' -> 129  -- Backspace convention used by many terminals

    -- uppercase and lowercase letters, digits, punctuation, and spaces
    c | c >= ' ' && c <= '~' -> ord c
    _ -> 0

-- | Encode a numeric key code with the most significant bit first.
toKeyboardOutput :: Int -> KeyboardOutput
toKeyboardOutput code =
    bitAt <$> HackWord16F
        (15, 14, 13, 12, 11, 10, 9, 8,
          7,  6,  5,  4,  3,  2, 1, 0)
  where
    bitAt index =
        if testBit code index then One else Zero