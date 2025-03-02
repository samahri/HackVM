{-# LANGUAGE ScopedTypeVariables #-}
module Nand2Tetris.Memory (
    dff
   ,bit
--    , register'
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

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Memory
import Nand2Tetris.Gates (mux, dMux8Way, dMux8Way16, mux8Way16, dMux4Way, dMux4Way16, muxRam, mux8WayRam, mux4WayRam, dmux, dmux16)
import Nand2Tetris.Chips (inc16)
import BasicPrelude ((<$>), fst, snd, ($))
import Control.Applicative (Applicative, pure, (<*), liftA2)
import Control.Monad.Trans.State.Strict (State, get, put, runState)

{- 
    data flip flop
    > out(t) = in (t - 1)
-}
dff :: InputBit -> DFF
dff input = get <* put input

{-
    single bit register

    input params: in load
        if load == 1 -> out(t) = in (t - 1)
        if load == 0 -> out(t) = out(t-1)
-} 
bit :: InputBit -> Load -> DFF
bit input load = do
    prevOutput <- get
    let dffInput = mux (prevOutput, input) load
    dff dffInput -- returns previous state

{-
    16-bit register
    constructed using 8 single bit registers
-}

-- type RegisterState' = HackWord16F DFF
-- type Register' = State RegisterState' MemoryOutput 

register :: Input16 -> Load -> Register
register input16 load = do
    registerState <- get
    let (registerOutput, nextCycleOutput) = operateMemoryMachine bit input16 (pure load) registerState 
    put nextCycleOutput
    pure registerOutput

-- TODO: look into if memory needs to be a Monad to be sequenced using (>>) or simply a state transition function using runState
-- register' :: Input16 -> Load -> Register'
-- register' input16 load = do
--     registerState <- get
--     let nextCycleOutput = liftA2 (>>) registerState (fmap (`bit` load) input16)
--         registerOutput = fmap (`evalState` Zero) nextCycleOutput 
--     put nextCycleOutput
--     pure registerOutput

{-
    RAM8 - 8 x 16 bit RAM
    constructed using 8 registers
-}
ram8 :: RAM8Address -> Input16 -> Load  -> RAM8
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
ram64 :: RAM64Address -> Input16 -> Load -> RAM64
ram64 (sel5, sel4, sel3, sel2, sel1, sel0) input16 load = do
    ram64State <- get
    let inputBus = dMux8Way16 input16 ram8Selector
        loadArr = dMux8Way load ram8Selector
        memroyFunction = ram8 ram8MemoryBus

        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram64State

        ram8Output = mux8WayRam registerOutput ram8Selector

    put nextCycleOutput
    pure ram8Output
    where
        ram8MemoryBus = (sel2, sel1, sel0)
        ram8Selector = (sel5, sel4, sel3)

{-
    RAM512 - 512 x 16 bit RAM
    constructed using 8 RAM64
-}
ram512 :: RAM512Address -> Input16 -> Load -> RAM512
ram512 (sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) input16 load = do
    ram512State <- get
    let inputBus = dMux8Way16 input16 ram64Selector
        loadArr = dMux8Way load ram64Selector
        memroyFunction = ram64 ram64MemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram512State
        
        ram64Output = mux8WayRam registerOutput ram64Selector

    put nextCycleOutput
    pure ram64Output
    where
        ram64MemoryBus = (sel5, sel4, sel3, sel2, sel1, sel0)
        ram64Selector = (sel8, sel7, sel6)

{-
    RAM4K - 4096 x 16 bit RAM
    constructed using 8 RAM512
-}
ram4K :: RAM4KAddress -> Input16 -> Load -> RAM4k
ram4K (sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) input16 load = do
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
        ram512MemoryBus = (sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0)
        ram512Selector = (sel11, sel10, sel9)

{-
    RAM16K - 16384 x 16 bit RAM
    constructed using 4 RAM4ks
-}
ram16K :: RAM16KAddress -> Input16 -> Load -> RAM16k
ram16K (sel13, sel12, sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) input16 load = do
    ram16KState <- get
    let inputBus = dMux4Way16 input16 ram4KSelector
        loadArr = dMux4Way load ram4KSelector

        memroyFunction = ram4K ram4KMemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr ram16KState
        
        ram4kOutput = mux4WayRam registerOutput ram4KSelector

    put nextCycleOutput
    pure ram4kOutput
    where
        ram4KMemoryBus = (sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0)
        ram4KSelector = (sel13, sel12)

{-
    Program Counter

    When inc==1, the counter increments its state in every clock cycle, effecting the operation PC++. 
    If we want to reset the counter to 0, we assert the reset bit; 
    if we want to set the counter to the value v, we put v in the in input and assert the load bit, as we normally do with registers.
    otherwise out(t) = out (t-1)
-} 
type Inc = Bit
type CounterCtrl = (Load, Inc, Reset)

pc :: Input16 -> CounterCtrl -> Register
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
rom32K :: ROMAddress -> ROM32k
rom32K (HackWord16F (_, addr14, addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0)) = do
    rom32KState <- get
    let ram16KOutput = muxRam rom32KState addr14
        ram4kOutput = mux4WayRam ram16KOutput ram4KSelector
        ram512Output = mux8WayRam ram4kOutput ram512Selector
        ram64Output = mux8WayRam ram512Output ram64Selector
        ram8Output = mux8WayRam ram64Output ram8Selector
        registerOutput = mux8Way16 ram8Output registerSelector

    pure registerOutput
    where
        ram4KSelector = (addr13, addr12)
        ram512Selector = (addr11, addr10, addr9)
        ram64Selector = (addr8, addr7, addr6)
        ram8Selector = (addr5, addr4, addr3)
        registerSelector = (addr2, addr1, addr0)

-- utility function to load the ROM
loadROM32K :: ROMAddress -> Input16 -> ROM32k
loadROM32K (HackWord16F (_, addr14, addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0)) input16 = do
    rom32KState <- get

    let inputBus = dmux16 input16 addr14
        loadArr = dmux One addr14

        memroyFunction = ram16K ram16KMemoryBus
        
        (registerOutput, nextCycleOutput) = operateMemoryMachine memroyFunction inputBus loadArr rom32KState

        ram16KOutput = muxRam registerOutput addr14 

    put nextCycleOutput
    pure ram16KOutput
    where
        ram16KMemoryBus = (addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0) 

operateMemoryMachine :: forall s a f. Applicative f => (a -> Load -> State s a) -> f a -> f Load -> f s -> (f a, f s)
operateMemoryMachine memoryUnit inputBus loadBus memState = sequence $ liftA2 runState memoryBus memState
    where
        memoryBus :: f (State s a)
        memoryBus = liftA2 memoryUnit inputBus loadBus
        sequence :: f (a, s) -> (f a, f s)
        sequence z = (fst <$> z, snd <$> z)
