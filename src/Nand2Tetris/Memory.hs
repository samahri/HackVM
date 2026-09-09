{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Nand2Tetris.Memory (
    dff
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
   ,memoryBank
   ,operateMemoryMachine
) where
import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Memory
import Nand2Tetris.Gates (mux, mux16, dMux8Way, mux8Way16, dMux4Way, muxRam, mux8WayRam, mux4WayRam, dmux)
import Nand2Tetris.Chips (inc16)

import BasicPrelude (($))
import Data.Function ((&))
import Data.Functor (unzip)
import Control.Applicative (Applicative, pure, liftA3)
import Control.Monad.Trans.State.Strict (State, state, get, gets, runState)

{- 
    data flip flop
    > out(t) = in (t - 1)
-}
dff :: InputBit -> DFF
dff input = state (, input)

{-
    single bit register

    input params: in load
        if load == 1 -> out(t) = in (t - 1)
        if load == 0 -> out(t) = out(t-1)
-} 
bit :: InputBit -> Load -> DFF
bit input load =
    state $ \old -> (old, mux (old, input) load)

{-
    16-bit register
    constructed using 8 single bit registers
-}

register :: Input16 -> Load -> Register
register input load = 
    state $ operateMemoryMachine bit input (pure load)

{-
    RAM8 - 8 x 16 bit RAM
    constructed using 8 registers
-}
ram8 :: RAM8Address -> Input16 -> Load  -> RAM8
ram8 = 
    memoryBank dMux8Way mux8Way16 register
        
{-
    RAM64 - 64 x 16 bit RAM
    constructed using 8 RAM8
-}
ram64 :: RAM64Address -> Input16 -> Load -> RAM64
ram64 (sel5, sel4, sel3, sel2, sel1, sel0) = 
    let
      ram8MemoryBus = (sel2, sel1, sel0)
      ram8Selector  = (sel5, sel4, sel3)
    in
      memoryBank 
        dMux8Way 
        mux8WayRam
        (ram8 ram8MemoryBus)
        ram8Selector       

{-
    RAM512 - 512 x 16 bit RAM
    constructed using 8 RAM64
-}
ram512 :: RAM512Address -> Input16 -> Load -> RAM512
ram512 (sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) = 
    let
      ram64MemoryBus  = (sel5, sel4, sel3, sel2, sel1, sel0)
      ram64Selector   = (sel8, sel7, sel6)
    in
      memoryBank 
        dMux8Way 
        mux8WayRam
        (ram64 ram64MemoryBus)
        ram64Selector

{-
    RAM4K - 4096 x 16 bit RAM
    constructed using 8 RAM512
-}
ram4K :: RAM4KAddress -> Input16 -> Load -> RAM4k
ram4K (sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) = 
    let
      ram512MemoryBus = (sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0)
      ram512Selector  = (sel11, sel10, sel9)
    in
      memoryBank
        dMux8Way
        mux8WayRam
        (ram512 ram512MemoryBus)
        ram512Selector

{-
    RAM16K - 16384 x 16 bit RAM
    constructed using 4 RAM4ks
-}
ram16K :: RAM16KAddress -> Input16 -> Load -> RAM16k
ram16K (sel13, sel12, sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0) =
    let
      ram4KMemoryBus  = (sel11, sel10, sel9, sel8, sel7, sel6, sel5, sel4, sel3, sel2, sel1, sel0)
      ram4KSelector   = (sel13, sel12)
    in
      memoryBank
        dMux4Way
        mux4WayRam
        (ram4K ram4KMemoryBus)
        ram4KSelector

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
pc input (load, inc, reset) = do
    current <- get

    let afterInc  = mux16 (current, inc16 current) inc
        afterLoad = mux16 (afterInc, input) load
        next      = mux16 (afterLoad, pure Zero) reset

    register next One

{-
    32KB Read Only Memory
    Constructed from two 16KB RAM
-}
rom32K :: ROMAddress -> ROM32k
rom32K 
  (HackWord16F 
    (_, addr14, addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0)) =
      let 
        ram4KSelector = (addr13, addr12)
        ram512Selector = (addr11, addr10, addr9)
        ram64Selector = (addr8, addr7, addr6)
        ram8Selector = (addr5, addr4, addr3)
        registerSelector = (addr2, addr1, addr0)
      in
        -- gets :: (s -> a) -> State s a
        -- `gets f` means "apply f to the current state and return the result."
        gets $ \rom ->
          rom
              -- x & f === f x
              & (`muxRam` addr14)
              & (`mux4WayRam` ram4KSelector)
              & (`mux8WayRam` ram512Selector)
              & (`mux8WayRam` ram64Selector)
              & (`mux8WayRam` ram8Selector)
              & (`mux8Way16` registerSelector)

loadROM32K :: ROMAddress -> Input16 -> ROM32k
loadROM32K (HackWord16F (_, addr14, addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0)) input =
    let
      ram16KMemoryBus = (addr13, addr12, addr11, addr10, addr9, addr8, addr7, addr6, addr5, addr4, addr3, addr2, addr1, addr0) 
    in
      memoryBank
        dmux
        muxRam
        (ram16K ram16KMemoryBus)
        addr14
        input
        One

{-
  Helper function to construct an addressable memory bank from smaller memory units.

  Broadcast the input to every child and demultiplex the load signal
  so that only the addressed child can write. Step each child using its
  own state, select the addressed child's output, and store all next states.

  The address selects a child within this bank. If the children are RAMs,
  their internal address must already be supplied to the child function.

  Children must retain their contents when load is Zero. With the
  register/RAM implementations, the returned output is the selected
  word before this step's write; the write becomes visible next step.
-}
memoryBank
    :: Applicative f
    => (Load -> addr -> f Load)       -- Route the load signal
    -> (f d -> addr -> d)             -- Select an output
    -> (d -> Load -> State s d)       -- Operate one child
    -> addr
    -> d
    -> Load
    -> State (f s) d
memoryBank demux selecter child addr input load =
    state $ \oldStates ->
        let loads = demux load addr

            (outputs, newStates) =
                operateMemoryMachine
                    child
                    (pure input)
                    loads
                    oldStates

        in (selecter outputs addr, newStates)

{-
  Advance a collection of independent memory units by one simulated
  clock step.

  Each unit receives the input, load signal, and current state at its
  corresponding bus position. Running these units produces a pair:
  (outputs for this step, states for the next step).
-}
operateMemoryMachine 
    :: forall s a f. Applicative f 
    => (a -> Load -> State s a) 
    -> f a 
    -> f Load 
    -> f s 
    -> (f a, f s)
operateMemoryMachine memoryUnit inputBus loadBus memState = 
    unzip $ liftA3 step inputBus loadBus memState

    where
      step input load = runState (memoryUnit input load)
        