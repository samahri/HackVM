module Nand2Tetris.HackComputer (
    cpu
    -- , keyboard
    -- , ram64K
    -- , hackComputer
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Memory
import Nand2Tetris.Chips
import Nand2Tetris.Gates
import Control.Monad.Trans.State.Strict (State, get, put, execState)
import BasicPrelude (pure)

-- type Input16 = HackWord16
-- type Output16 = HackWord16

type Reset = Bit
type Instruction = HackWord16 -- value read from ROM
type MInput = HackWord16 -- value read from RAM (M register or RAM[A])

type MAddress = HackWord16 -- 15-bit address; last bit is ignored
type MOutput = HackWord16 -- value written to RAM[MAddress]
type MWrite = Bit
type PCOutput = HackWord16 -- 15-bit address; last bit is ignored

type ARegister = HackWord16
type DRegister = HackWord16
type PCRegister = HackWord16

type CpuRegisters = (ARegister, DRegister, PCRegister)
type CpuState = (MAddress, MOutput, MWrite, PCOutput)
type CpuOUtput = State CpuRegisters CpuState 

cpu :: MInput -> Instruction -> Reset -> CpuOUtput
cpu mInput instruction reset = do
    (aReg, dReg, pcOutput) <- get

    let aluInput = mux16 (aReg, mInput) aluMuxInputCtrl
        (outM, zeroFlag, negativeFlag) = alu (dReg, aluInput) aluCtrl
        aRegInput = mux16 (instruction, outM) aRegInputMuxControlBit
        condZero = and (jZero, and (not negativeFlag, zeroFlag))
        condNeg = and (jNeg, and (negativeFlag, not zeroFlag))
        condPos = and (jPos, and (not negativeFlag, not zeroFlag ))
        unCond = and (jNeg, and (jPos, jZero ))
        jumpLogic = or (or (condZero, condNeg), or (condPos, unCond))
        (load, inc) = let result = and (aRegInputMuxControlBit, jumpLogic) in (result, not result) -- (result, not result) is inc
        pcNextCycle = execState (pc aReg (load , inc, reset)) pcOutput
        aRegNextCycle = execState (register aRegInput aRegLoad) aReg
        dRegNextCycle = execState (register outM dRegLoad) dReg

    put (aRegNextCycle, dRegNextCycle, pcNextCycle)
    pure (aReg, outM, memoryWrite, pcOutput)
    where
        -- control bits
        aRegInputMuxControlBit = getOpcode instruction
        (aluMuxInputCtrl, aluCtrl) = getCompBits instruction
        (aRegLoadCtrl, dRegLoadCtrl, memoryWrite) = getDestBit instruction
        (jNeg, jZero, jPos) = getJumpBits instruction

        aRegLoad = mux (One, aRegLoadCtrl) aRegInputMuxControlBit
        dRegLoad = mux (Zero, dRegLoadCtrl) aRegInputMuxControlBit


-- ram16K :: RAM16KAddress -> Input16 -> Load -> RAM16kOutput
-- ram16K (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13) input16 load = do
--     ram16KState <- get
    

getOpcode :: Instruction -> Bit
getOpcode (HackWord16F (x, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) = x

getCompBits :: Instruction -> (Bit, (Bit, Bit, Bit, Bit, Bit, Bit))
getCompBits (HackWord16F (_, _, _, a, c1, c2, c3, c4, c5, c6, _, _, _, _, _, _)) = (a, (c1, c2, c3, c4, c5, c6))

getDestBit :: Instruction -> (Bit, Bit, Bit)
getDestBit (HackWord16F (_, _, _, _, _, _, _, _, _, _, d1, d2, d3, _, _, _)) = (d1, d2, d3)

getJumpBits :: Instruction -> (Bit, Bit, Bit)
getJumpBits (HackWord16F (_, _, _, _, _, _, _, _, _, _, _, _, _, j1, j2, j3)) = (j1, j2, j3)



