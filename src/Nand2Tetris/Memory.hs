{-# LANGUAGE ScopedTypeVariables #-}
module Nand2Tetris.Memory (
    DFF
   ,DFF16
   ,dff
   ,bit
   ,register
   ,ram8
   ,ram64
   ,ram512
   ,ram4K
   ,ram16K
   ,pc
) where

import Nand2Tetris.Types.Bit(Bit(..))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Utils
import Nand2Tetris.Types.Bus
import Nand2Tetris.Gates (mux, dMux8Way, dMux8Way16, mux8Way16, dMux4Way, dMux4Way16, mux8WayRam, mux4WayRam)
import Nand2Tetris.Chips (inc16)
import BasicPrelude (($), pure, (<$>), replicate, (<*))
import Control.Monad.Trans.State.Strict (State, get, put, execState)
import Data.List (zipWith)

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
    -- TODO use classes instead of different functions
    let regStateList = toList registerState
        nextCycleOutput = operateMemoryMachine bit inputList loadList regStateList 
    put (toHackWord16 nextCycleOutput) 
    pure registerState
        where
            loadList :: [Load]
            loadList = replicate 16 load

            inputList :: [Bit]
            inputList = toList input16


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
    let inputList = bus8ToList $ dMux8Way16 input16 addr
        loadArr = bus8ToList $ dMux8Way load addr
        ram8StateList = bus8ToList ram8State
        nextCycleOutput = operateMemoryMachine register inputList loadArr ram8StateList
    put (toBus8 nextCycleOutput)
    pure $ mux8Way16 ram8State addr
        
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
    let inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram64StateList = bus8ToList ram64State
        ram8Output = mux8WayRam ram64State (sel0, sel1, sel2)
        ram64Output = mux8Way16 ram8Output (sel3, sel4, sel5)
        nextCycleOutput = operateMemoryMachine (ram8 (sel3, sel4, sel5)) inputList loadArr ram64StateList
    put (toBus8 nextCycleOutput)
    pure ram64Output 

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
    let inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram512List = bus8ToList ram512State
        
        nextCycleOutput = operateMemoryMachine (ram64 (sel3, sel4, sel5, sel6, sel7, sel8)) inputList loadArr ram512List
        -- TODO: verify names of variables
        ram64Output = mux8WayRam ram512State (sel0, sel1, sel2)
        ram16Output = mux8WayRam ram64Output (sel3, sel4, sel5)
        ram8Output = mux8Way16 ram16Output (sel6, sel7, sel8)

    put (toBus8 nextCycleOutput)
    pure ram8Output 

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
        inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2) 
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram4KList = bus8ToList ram4KState
        
        nextCycleOutput = operateMemoryMachine (ram512 (sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11)) inputList loadArr ram4KList
        
        ram512Output = mux8WayRam ram4KState (sel0, sel1, sel2)
        ram64Output = mux8WayRam ram512Output (sel3, sel4, sel5)
        ram8Output = mux8WayRam ram64Output (sel6, sel7, sel8)
        registerOutput = mux8Way16 ram8Output (sel9, sel10, sel11)

    put (toBus8 nextCycleOutput)
    pure registerOutput

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
    let inputList = bus4ToList $ dMux4Way16 input16 (sel0, sel1)
        loadArr = bus4ToList $ dMux4Way load (sel0, sel1)
        ram16kList = bus4ToList ram16KState
        
        nextCycleOutput = operateMemoryMachine (ram4K (sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13)) inputList loadArr ram16kList
        
        ram4kOutput = mux4WayRam ram16KState (sel0, sel1)
        ram512Output = mux8WayRam ram4kOutput (sel2, sel3, sel4)
        ram64Output = mux8WayRam ram512Output (sel5, sel6, sel7)
        ram8Output = mux8WayRam ram64Output (sel8, sel9, sel10)
        registerOutput = mux8Way16 ram8Output (sel11, sel12, sel13)

    put (toBus4 nextCycleOutput)
    pure registerOutput


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
    case ctrl of
        (_, _, One) -> register zeros One -- reset the counter to zero
        (One, _, _) -> register input16 One
        (_, One, _) -> do
            incrementedReg <- inc16 <$> get
            register incrementedReg One
        (Zero, Zero, Zero) -> register input16 Zero
    where
        zeros = toHackWord16 $ replicate 16 Zero

operateMemoryMachine :: forall s a. (a -> Load -> State s a) -> [a] -> [Load] -> [s] -> [s]
operateMemoryMachine memoryUnit inputList loadList = zipWith execState memoryList
    where
        memoryList :: [State s a]
        memoryList = zipWith memoryUnit inputList loadList 