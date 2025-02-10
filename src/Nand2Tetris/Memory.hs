{-# LANGUAGE ScopedTypeVariables #-}
module Nand2Tetris.Memory (
    DFF
   ,DFF16
   ,Ram4kState
   ,Ram16kState
   ,dff
   ,bit
   ,register
   ,ram8
   ,ram64
   ,ram512
   ,ram4K
   ,ram16K
   ,pc
   ,rom32K
   ,loadROM32K
   , operateMemoryMachine
) where

import Nand2Tetris.Types.Bit(Bit(..))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bus
import Nand2Tetris.Gates (mux, dMux8Way, dMux8Way16, mux8Way16, dMux4Way, dMux4Way16, muxRam, mux8WayRam, mux4WayRam, dmux, dmux16)
import Nand2Tetris.Chips (inc16)
import BasicPrelude ((<$>), fst, snd, ($))
import Control.Applicative (Applicative, pure, (<*), liftA2)
import Control.Monad.Trans.State.Strict (State, get, put, execState)

type Input = Bit
type Output = Bit
type Input16 = HackWord16
type Output16 = HackWord16

{- 
    data flip flop
    > out(t) = in (t - 1)
-}

type DFF = State Bit Output

dff :: Input -> DFF
dff input = get <* put input

{-
    single bit register

    input params: in load
        if load == 1 -> out(t) = in (t - 1)
        if load == 0 -> out(t) = out(t-1)
-} 
type Load = Bit

bit :: Input -> Load -> DFF
bit input load = do
    prevOutput <- get
    let dffInput = mux (prevOutput, input) load
    dff dffInput -- returns previous state

{-
    16-bit register
    constructed using 8 single bit registers
-}

type MemoryState = HackWord16

type DFF16 = State MemoryState Output16 

register :: Input16 -> Load -> DFF16
register input16 load = do
    registerState <- get
    let (registerOutput, nextCycleOutput) = operateMemoryMachine bit input16 (pure load) registerState 
    put nextCycleOutput
    pure registerOutput

{-
    RAM8 - 8 x 16 bit RAM
    constructed using 8 registers
-}

type RAM8Address = (Bit, Bit, Bit)
type Ram8State = Bus8Way MemoryState
type RAM8Output = State Ram8State Output16

ram8 :: RAM8Address -> Input16 -> Load  -> RAM8Output
ram8 addr input16 load = do
    ram8State <- get

    let inputBus = dMux8Way16 input16 addr
        loadArr = dMux8Way load addr
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine register inputBus loadArr ram8State
        
        ram8Output = mux8Way16 registerOutput addr
    put nextCycleOutput
    pure ram8Output
        
{-
    RAM64 - 64 x 16 bit RAM
    constructed using 8 RAM8
-}

type RAM64Address = (Bit, Bit, Bit, Bit, Bit, Bit)
type Ram64State = Bus8Way Ram8State
type RAM64Output = State Ram64State Output16

ram64 :: RAM64Address -> Input16 -> Load -> RAM64Output
ram64 (sel0, sel1, sel2, sel3, sel4, sel5) input16 load = do
    ram64State <- get
    let inputBus = dMux8Way16 input16 ram8Selector
        loadArr = dMux8Way load ram8Selector
        memroyFunction = ram8 ram8MemoryBus

        -- nextCycleOutput = operateMemoryMachine memroyFunction inputBus loadArr ram64State
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram64State

        ram8Output = mux8WayRam registerOutput ram8Selector

    put nextCycleOutput
    pure ram8Output
    where
        ram8MemoryBus = (sel0, sel1, sel2)
        ram8Selector = (sel3, sel4, sel5)

{-
    RAM512 - 512 x 16 bit RAM
    constructed using 8 RAM64
-}
type RAM512Address = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type Ram512State = Bus8Way Ram64State
type RAM512Output = State Ram512State Output16

ram512 :: RAM512Address -> Input16 -> Load -> RAM512Output
ram512 (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8) input16 load = do
    ram512State <- get
    let inputBus = dMux8Way16 input16 ram64Selector
        loadArr = dMux8Way load ram64Selector
        memroyFunction = ram64 ram64MemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram512State
        
        ram64Output = mux8WayRam registerOutput ram64Selector

    put nextCycleOutput
    pure ram64Output
    where
        ram64MemoryBus = (sel0, sel1, sel2, sel3, sel4, sel5)
        ram64Selector = (sel6, sel7, sel8)
        
          
{-
    RAM4K - 4096 x 16 bit RAM
    constructed using 8 RAM512
-}
type RAM4KAddress = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type Ram4kState = Bus8Way Ram512State
type RAM4kOutput = State Ram4kState Output16

ram4K :: RAM4KAddress -> Input16 -> Load -> RAM4kOutput
ram4K (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11) input16 load = do
    ram4KState <- get
    let 
        inputBus = dMux8Way16 input16 ram512Selector
        loadArr = dMux8Way load ram512Selector
        memroyFunction = ram512 ram512MemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram4KState
        
        ram512Output = mux8WayRam registerOutput ram512Selector

    put nextCycleOutput
    pure ram512Output
    where
        ram512MemoryBus = (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8)
        ram512Selector = (sel9, sel10, sel11)

{-
    RAM16K - 16384 x 16 bit RAM
    constructed using 4 RAM4ks
-}
type RAM16KAddress = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type Ram16kState = Bus4Way Ram4kState
type RAM16kOutput = State Ram16kState Output16

ram16K :: RAM16KAddress -> Input16 -> Load -> RAM16kOutput
ram16K (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13) input16 load = do
    ram16KState <- get
    let inputBus = dMux4Way16 input16 ram4KSelector
        loadArr = dMux4Way load ram4KSelector

        memroyFunction = ram4K ram4KMemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram16KState
        
        ram4kOutput = mux4WayRam registerOutput ram4KSelector

    put nextCycleOutput
    pure ram4kOutput
    where
        ram4KMemoryBus = (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11)
        ram4KSelector = (sel12, sel13)

{-
    Program Counter

    When inc==1, the counter increments its state in every clock cycle, effecting the operation PC++. 
    If we want to reset the counter to 0, we assert the reset bit; 
    if we want to set the counter to the value v, we put v in the in input and assert the load bit, as we normally do with registers.
    otherwise out(t) = out (t-1)
-} 
type Inc = Bit
type Reset = Inc
type CounterCtrl = (Load, Inc, Reset)

pc :: Input16 -> CounterCtrl -> DFF16
pc input16 ctrl = do
    -- todo: figure out the priority of this
    -- TODO: use logic components
    case ctrl of
        (_, _, One) -> register zeros One -- reset the counter to zero
        (One, _, _) -> register input16 One
        (_, One, _) -> do
            incrementedReg <- inc16 <$> get
            register incrementedReg One
        (Zero, Zero, Zero) -> register input16 Zero
    where
        zeros = pure Zero

{-
    32KB Read Only Memory
    Constructed from two 16KB RAM
-}
type ROMAddress = HackWord16 -- 15-bit address
type ROM32kState = Bus2Way Ram16kState
type ROM32kOutput = State ROM32kState Output16

rom32K :: ROMAddress -> ROM32kOutput
rom32K (HackWord16F (addr0, addr1, addr2, addr3, addr4, addr5, addr6, addr7, addr8, addr9, addr10, addr11, addr12, addr13, addr14, _)) = do
    rom32KState <- get
    let ram16KOutput = muxRam rom32KState addr14
        ram4kOutput = mux4WayRam ram16KOutput ram4KSelector
        ram512Output = mux8WayRam ram4kOutput ram512Selector
        ram64Output = mux8WayRam ram512Output ram64Selector
        ram8Output = mux8WayRam ram64Output ram8Selector
        registerOutput = mux8Way16 ram8Output registerSelector

    pure registerOutput
    where
        ram4KSelector = (addr12, addr13)
        ram512Selector = (addr9, addr10, addr11)
        ram64Selector = (addr6, addr7, addr8)
        ram8Selector = (addr3, addr4, addr5)
        registerSelector = (addr0, addr1, addr2)

-- utility function to load the ROM
loadROM32K :: ROMAddress -> Input16 -> ROM32kOutput
loadROM32K (HackWord16F (addr0, addr1, addr2, addr3, addr4, addr5, addr6, addr7, addr8, addr9, addr10, addr11, addr12, addr13, addr14, _)) input16 = do
    rom32KState <- get

    let inputBus = dmux16 input16 addr14
        loadArr = dmux One addr14

        memroyFunction = ram16K ram16KMemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr rom32KState

        ram16KOutput = muxRam registerOutput addr14 
        -- ram4kOutput = mux4WayRam ram16KOutput ram4KSelector
        -- ram512Output = mux8WayRam ram4kOutput ram512Selector
        -- ram64Output = mux8WayRam ram512Output ram64Selector
        -- ram8Output = mux8WayRam ram64Output ram8Selector
        -- registerOutput = mux8Way16 ram8Output registerSelector

    put nextCycleOutput
    pure ram16KOutput
    where
        ram16KMemoryBus = (addr0, addr1, addr2, addr3, addr4, addr5, addr6, addr7, addr8, addr9, addr10, addr11, addr12, addr13) 

operateMemoryMachine :: forall s a f. Applicative f => (a -> Load -> State s a) -> f a -> f Load -> f s -> (f a, f s)
operateMemoryMachine memoryUnit inputBus loadBus memState = sequence $ liftA2 runState memoryBus memState -- f (s, a)
    where
        memoryBus :: f (State s a)
        memoryBus = liftA2 memoryUnit inputBus loadBus
        sequence :: f (a, s) -> (f a, f s)
        sequence z = (fst <$> z, snd <$> z)
