{-# LANGUAGE ScopedTypeVariables #-}
module Nand2Tetris.HackComputer (
    cpu
    , mainMemory
    -- , hackComputer
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Memory
import Nand2Tetris.InputOutput
import Nand2Tetris.Chips
import Nand2Tetris.Gates
import Nand2Tetris.Types.Bus
import Control.Monad.Trans.State.Strict (State, StateT, get, put, execState)
import BasicPrelude (pure, IO, (==))
import Control.Monad.IO.Class (liftIO)

type Input16 = HackWord16
type Output16 = HackWord16

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
type CpuOutput = (MAddress, MOutput, MWrite, PCOutput)
type CpuState = State CpuRegisters CpuOutput 

cpu :: MInput -> Instruction -> Reset -> CpuState
cpu mInput instruction reset = do
    (aReg, dReg, pcOutput) <- get

    let aluInput = mux16 (aReg, mInput) aluMuxInputCtrl
        (outM, zeroFlag, negativeFlag) = alu (dReg, aluInput) aluCtrl
        aRegInput = mux16 (instruction, outM) aRegInputMuxControlBit
        
        pcControl = let (load, inc) = getPCCtrl instruction zeroFlag negativeFlag in (load, inc, reset)
        pcNextCycle = execState (pc aReg pcControl) pcOutput
        aRegNextCycle = execState (register aRegInput aRegLoad) aReg
        dRegNextCycle = execState (register outM dRegLoad) dReg

    put (aRegNextCycle, dRegNextCycle, pcNextCycle)
    pure (aReg, outM, memoryWrite, pcOutput)
    where
        -- control bits
        aRegInputMuxControlBit = getOpcode instruction
        (aluMuxInputCtrl, aluCtrl) = getCompBits instruction
        (aRegLoadCtrl, dRegLoadCtrl, memoryWrite) = getDestBit instruction

        aRegLoad = mux (One, aRegLoadCtrl) aRegInputMuxControlBit
        dRegLoad = mux (Zero, dRegLoadCtrl) aRegInputMuxControlBit

{-
    main memory

    address space
    x000 0000 0000 0000 -> x011 1111 1111 1111 : RAM16 cell
    x100 0000 0000 0000 -> x101 1111 1111 1111 : screen space
    x110 0000 0000 0000: keyboard register

    x0xx xxxx xxxx xxxx : ram address space
    x10x xxxx xxxx xxxx : screen space
    x11x xxxx xxxx xxxx : keyboard

-}

type Load = Bit

type MemoryAddress = HackWord16 -- 15 bit address
type MemoryState = Bus2Way Ram16kState

type MemoryOutput = StateT MemoryState (StateT Ram16kState IO) Output16

mainMemory :: MemoryAddress -> Input16 -> Load -> MemoryOutput
mainMemory (HackWord16F (_, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13, sel14, sel15) )input16 load = do
    memoryState <- get
    
    let inputBus = dmux16 input16 ram16KSelector
        loadArr = dmux load ram16KSelector

    (registerOutput, nextCycleOutput) <- 
        if ram16KSelector == Zero then do
            let memroyFunction = ram16K ram16KMemoryBus
                (output, newState) = operateMemoryMachine memroyFunction inputBus loadArr memoryState
            pure (muxRam output ram16KSelector, newState)
        else 
            if screenSelector == Zero then do
                let memroyFunction = screen screenBus
                    (output, newState) = operateMemoryMachine memroyFunction inputBus loadArr memoryState 
                pure (muxRam output ram16KSelector, newState)
            else do
                kb <- liftIO keyboard
                pure (kb, memoryState)

    put nextCycleOutput
    pure registerOutput
    where
        ram16KMemoryBus = (sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13, sel14, sel15)
        screenBus = (sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13, sel14, sel15) 
        ram16KSelector = sel1
        screenSelector = sel2

type ComputerMemory = (MemoryState, ROM32kState)
type ComputerOutput = StateT ComputerMemory IO ()

-- hackComputer :: Reset -> ComputerOutput
-- hackComputer reset = _
--     -- fetch execute loop
    
getPCCtrl :: Instruction -> Bit -> Bit -> (Bit, Bit)
getPCCtrl instruction zeroFlag negativeFlag = (result, not result)
    where
        result = and (isCInstruction, jumpLogic)
        isCInstruction = getOpcode instruction
        jumpLogic = or (or (condZero, condNeg), or (condPos, unCond))
            where
                (jNeg, jZero, jPos) = getJumpBits instruction
                condZero = and (jZero, and (not negativeFlag, zeroFlag))
                condNeg = and (jNeg, and (negativeFlag, not zeroFlag))
                condPos = and (jPos, and (not negativeFlag, not zeroFlag ))
                unCond = and (jNeg, and (jPos, jZero ))

getOpcode :: Instruction -> Bit
getOpcode (HackWord16F (x, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) = x

getCompBits :: Instruction -> (Bit, AluCtrl)
getCompBits (HackWord16F (_, _, _, a, c1, c2, c3, c4, c5, c6, _, _, _, _, _, _)) = (a, AluCtrl {zx = c1, nx = c2, zy = c3, ny = c4, f = c5, no = c6})

getDestBit :: Instruction -> (Bit, Bit, Bit)
getDestBit (HackWord16F (_, _, _, _, _, _, _, _, _, _, d1, d2, d3, _, _, _)) = (d1, d2, d3)

getJumpBits :: Instruction -> (Bit, Bit, Bit)
getJumpBits (HackWord16F (_, _, _, _, _, _, _, _, _, _, _, _, _, j1, j2, j3)) = (j1, j2, j3)


