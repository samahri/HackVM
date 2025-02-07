module Nand2Tetris.HackComputer (
    cpu
    -- , screen
    -- , keyboard
    -- , ram64K
    -- , hackComputer
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
-- import Nand2Tetris.Memory (rom32K)

import BasicPrelude (undefined)

-- type Input16 = HackWord16
-- type Output16 = HackWord16

type Reset = Bit
type Instruction = HackWord16 -- value read from ROM
type MInstruction = HackWord16 -- value read from RAM (M register or RAM[A])

type MAddress = HackWord16 -- 15-bit address; last bit is ignored
type MOutput = HackWord16 -- value written to RAM[MAddress]
type MWrite = Bit
type PC = HackWord16 -- 15-bit address; last bit is ignored

cpu :: MInstruction -> Instruction -> Reset -> (MAddress, MOutput, MWrite, PC)
cpu mInstruction instruction reset = undefined

