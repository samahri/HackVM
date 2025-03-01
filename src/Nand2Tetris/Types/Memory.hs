module Nand2Tetris.Types.Memory (
    DFF
   ,Register
   ,MemoryOutput
   ,RAM8Address
   ,RAM8
   ,RAM8State
   ,RAM64Address
   ,RAM512Address
   ,RAM64
   ,RAM64State
   ,RAM512
   ,RAM512State
   ,RAM4KAddress
   ,RAM4kState
   ,RAM4k
   ,RAM16KAddress
   ,RAM16kState
   ,RAM16k
   ,ROMAddress
   ,ROM32k
   ,ROM32kState
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bus
import Control.Monad.Trans.State.Strict (State)

type MemoryOutput = Output16

type DffStateBit = Bit
type DFF = State DffStateBit OutputBit

type RegisterState = HackWord16
type Register = State RegisterState MemoryOutput 

type RAM8Address = (Bit, Bit, Bit) -- todo: find a type for addresses
type RAM8State = Bus8Way RegisterState
type RAM8 = State RAM8State MemoryOutput

type RAM64Address = (Bit, Bit, Bit, Bit, Bit, Bit)
type RAM64State = Bus8Way RAM8State
type RAM64 = State RAM64State MemoryOutput

type RAM512Address = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type RAM512State = Bus8Way RAM64State
type RAM512 = State RAM512State MemoryOutput

type RAM4KAddress = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type RAM4kState = Bus8Way RAM512State
type RAM4k = State RAM4kState MemoryOutput

type RAM16KAddress = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type RAM16kState = Bus4Way RAM4kState
type RAM16k = State RAM16kState MemoryOutput

type ROMAddress = HackWord16 -- 15-bit address
type ROM32kState = Bus2Way RAM16kState
type ROM32k = State ROM32kState MemoryOutput
