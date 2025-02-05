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
import Nand2Tetris.Gates (mux, dMux8Way, dMux8Way16, mux8Way16, dMux4Way, dMux4Way16)
import Nand2Tetris.Chips (inc16)
import BasicPrelude (($), pure, (<$>), replicate, fmap)
import Control.Monad.Trans.State.Strict (State, get, put, execState)
import Data.List (zipWith)

type Input = Bit
type Output = Bit

type DFF = State Bit Output

{- 
    data flip flop
    > out(t) = in (t - 1)
-}
dff :: Input -> DFF
dff inpt = do --get <* put inpt
    previousState <- get
    put inpt
    pure previousState

type Load = Bit

{-
    single bit register

    input params: in load
        if load == 1 -> out(t) = in (t - 1)
        if load == 0 -> out(t) = out(t-1)
-} 
bit :: Input -> Load -> DFF
bit input load = do
    prevOutput <- get
    let dffInput = mux (prevOutput, input) load
    dff dffInput -- returns previous state

type Input16 = HackWord16
type Output16 = HackWord16

type MemoryState = HackWord16

type DFF16 = State MemoryState Output16 -- DFF16 is as primitive component similar to DFF

register :: Input16 -> Load -> DFF16
register input16 load = do
    registerState <- get
    let inputList = toList input16
        regStateList = toList registerState
        bitList = fmap (`bit` load) inputList
        nextCycleOutput = toHackWord16 $ zipWith execState bitList regStateList 
    put nextCycleOutput 
    pure registerState

type Address = (Bit, Bit, Bit)
type Ram8State = Bus8Way MemoryState

type RAM8Output = State Ram8State Output16

ram8 :: Input16 -> Address -> Load -> RAM8Output
ram8 input16 addr load = do
    ramState <- bus8ToList <$> get
    let nextCycleOutput = zipWith execState registerList ramState
    put (toBus8 nextCycleOutput)
    pure $ mux8Way16 (toBus8 ramState) addr
    where
        inputList = bus8ToList $ dMux8Way16 input16 addr
        loadArr = bus8ToList $ dMux8Way load addr
        registerList = zipWith register inputList loadArr
            
type Address6Bit = (Bit, Bit, Bit, Bit, Bit, Bit)
type Ram64State = Bus8Way Ram8State

type RAM64Output = State Ram64State Output16

-- implemented using 8 ram8
ram64 :: Input16 -> Address6Bit -> Load -> RAM64Output
ram64 input16 (sel0, sel1, sel2, sel3, sel4, sel5) load = do
    ram64State <- bus8ToList <$> get
    let nextCycleOutput = zipWith execState ram8List ram64State
    put (toBus8 nextCycleOutput)
    pure $ mux8Way16 (mux8WayRam (toBus8 ram64State) (sel0, sel1, sel2)) (sel3, sel4, sel5)
    where
        inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram8List = zipWith (\inp -> ram8 inp (sel3, sel4, sel5)) inputList loadArr


{-
    multiplexer to direct the output of a Ram8 in RAM64
    Ram64State -> Ram8State -> MemoryState
-}
type Sel = Bit

mux8WayRam :: Bus8Way a -> (Sel, Sel, Sel) -> a
mux8WayRam (Bus8Way(ram0, ram1, ram2, ram3, ram4, ram5, ram6, ram7)) sel = case sel of
    (Zero, Zero, Zero) -> ram0
    (Zero, Zero, One) -> ram1
    (Zero, One, Zero) -> ram2
    (Zero, One, One) -> ram3
    (One, Zero, Zero) -> ram4
    (One, Zero, One) -> ram5
    (One, One, Zero) -> ram6
    (One, One, One)  -> ram7

type Address9Bit = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type Ram512State = Bus8Way Ram64State

type RAM512Output = State Ram512State Output16

-- implemented using 8 ram8
ram512 :: Input16 -> Address9Bit -> Load -> RAM512Output
ram512 input16 (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8) load = do
    ram512State <- bus8ToList <$> get
    let nextCycleOutput = zipWith execState ram64List ram512State
        ram64Output = mux8WayRam (toBus8 ram512State) (sel0, sel1, sel2)

    put (toBus8 nextCycleOutput)
    pure $ mux8Way16 (mux8WayRam ram64Output (sel3, sel4, sel5)) (sel6, sel7, sel8)
    where
        inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram64List = zipWith (\inp -> ram64 inp (sel3, sel4, sel5, sel6, sel7, sel8)) inputList loadArr

type Address12Bit = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)

type Ram4kState = Bus8Way Ram512State

type RAM4kOutput = State Ram4kState Output16

ram4K :: Input16 -> Address12Bit -> Load -> RAM4kOutput
ram4K input16 (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11) load = do
    ram4KState <- bus8ToList <$> get
    let nextCycleOutput = zipWith execState ram4kList ram4KState
        ram512Output = mux8WayRam (toBus8 ram4KState) (sel0, sel1, sel2)
        ram64Output = mux8WayRam ram512Output (sel3, sel4, sel5)

    put (toBus8 nextCycleOutput)
    pure $ mux8Way16 (mux8WayRam ram64Output (sel6, sel7, sel8)) (sel9, sel10, sel11)
    where
        inputList = bus8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = bus8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram4kList = zipWith (\inp -> ram512 inp (sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11)) inputList loadArr



type Address14Bit = (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
type Ram16kState = Bus4Way Ram4kState

type RAM16kOutput = State Ram16kState Output16

ram16K :: Input16 -> Address14Bit -> Load -> RAM16kOutput
ram16K input16 (sel0, sel1, sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13) load = do
    ram16KState <- bus4ToList <$> get
    let nextCycleOutput = zipWith execState ram16kList ram16KState
        ram4kOutput = mux4WayRam (toBus4 ram16KState) (sel0, sel1)
        ram512Output = mux8WayRam ram4kOutput (sel2, sel3, sel4)
        ram64Output = mux8WayRam ram512Output (sel5, sel6, sel7)

    put (toBus4 nextCycleOutput)
    pure $ mux8Way16 (mux8WayRam ram64Output (sel8, sel9, sel10)) (sel11, sel12, sel13)
    where
        inputList = bus4ToList $ dMux4Way16 input16 (sel0, sel1)
        loadArr = bus4ToList $ dMux4Way load (sel0, sel1)
        ram16kList = zipWith (\inp -> ram4K inp (sel2, sel3, sel4, sel5, sel6, sel7, sel8, sel9, sel10, sel11, sel12, sel13)) inputList loadArr

mux4WayRam :: Bus4Way a -> (Sel, Sel) -> a
mux4WayRam (Bus4Way (a, b, c, d)) sel = case sel of
    (Zero, Zero) -> a
    (Zero, One) -> b
    (One, Zero) -> c
    (One, One) -> d   

type Inc = Bit
type Reset = Inc

type CounterCtrl = (Load, Inc, Reset)

{-
    counter
    When inc==1, the counter increments its state in every clock cycle, effecting the operation PC++. 
    If we want to reset the counter to 0, we assert the reset bit; 
    if we want to set the counter to the value v, we put v in the in input and assert the load bit, as we normally do with registers.
    otherwise out(t) = out (t-1)
-} 

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
