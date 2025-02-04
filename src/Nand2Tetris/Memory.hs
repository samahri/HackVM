module Nand2Tetris.Memory (
    DFF
   ,DFF16
   ,dff
   ,bit
   ,register
   ,ram8
   ,ram64
--    ,ram512
--    ,ram4K
--    ,ram16K
   ,pc
) where

import Nand2Tetris.Types.Bit(Bit(..))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Gates (mux, dMux8Way, dMux8Way16, mux8Way16)
import Nand2Tetris.Chips (inc16)
import BasicPrelude (($), pure, (<$>), replicate, fmap, error)
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
type Ram8State = (MemoryState, MemoryState, MemoryState, MemoryState, MemoryState, MemoryState, MemoryState, MemoryState) -- length is 8 word16

type RAM8Output = State Ram8State Output16

ram8 :: Input16 -> Address -> Load -> RAM8Output
ram8 input16 addr load = do
    ramState <- tuple8ToList <$> get
    let nextCycleOutput = zipWith execState registerList ramState
    put (to8Tuple nextCycleOutput)
    pure $ mux8Way16 ramState addr
    where
        inputList = tuple8ToList $ dMux8Way16 input16 addr
        loadArr = tuple8ToList $ dMux8Way load addr
        registerList = zipWith register inputList loadArr
            
type Address6Bit = (Bit, Bit, Bit, Bit, Bit, Bit)
type Ram64State = (Ram8State, Ram8State, Ram8State, Ram8State, Ram8State, Ram8State, Ram8State, Ram8State)  -- length is 8 ram8s

type RAM64Output = State Ram64State Output16

-- implemented using 8 ram8
ram64 :: Input16 -> Address6Bit -> Load -> RAM64Output
ram64 input16 (sel0, sel1, sel2, sel3, sel4, sel5) load = do
    ram64State <- tuple8ToList <$> get
    let nextCycleOutput = zipWith execState ram8List ram64State
    put (to8Tuple nextCycleOutput)
    pure $ mux8Way16 (tuple8ToList $ mux8WayRam64 (to8Tuple ram64State) (sel0, sel1, sel2)) (sel3, sel4, sel5)
    where
        inputList = tuple8ToList $ dMux8Way16 input16 (sel0, sel1, sel2)
        loadArr = tuple8ToList $ dMux8Way load (sel0, sel1, sel2)
        ram8List = zipWith (\inp -> ram8 inp (sel3, sel4, sel5)) inputList loadArr


{-
    multiplexer to direct the output of a Ram8 in RAM64
    Ram64State -> Ram8State -> MemoryState
-}
type Sel = Bit

mux8WayRam64 :: Ram64State -> (Sel, Sel, Sel) -> Ram8State
mux8WayRam64 (ram0, ram1, ram2, ram3, ram4, ram5, ram6, ram7) sel = case sel of
    (Zero, Zero, Zero) -> ram0
    (Zero, Zero, One) -> ram1
    (Zero, One, Zero) -> ram2
    (Zero, One, One) -> ram3
    (One, Zero, Zero) -> ram4
    (One, Zero, One) -> ram5
    (One, One, Zero) -> ram6
    (One, One, One)  -> ram7

tuple8ToList :: (a, a, a, a, a, a, a, a) -> [a]
tuple8ToList (x0, x1, x2, x3, x4, x5, x6, x7) = [x0, x1, x2, x3, x4, x5, x6, x7]

to8Tuple :: [a] -> (a, a, a, a, a, a, a, a)
to8Tuple [x0, x1, x2, x3, x4, x5, x6, x7] = (x0, x1, x2, x3, x4, x5, x6, x7)
to8Tuple _ = error "undefined"

-- tuple4ToList :: (a, a, a, a) -> [a]
-- tuple4ToList (x0, x1, x2, x3) = [x0, x1, x2, x3]

-- to4Tuple :: [a] -> (a, a, a, a)
-- to4Tuple [x0, x1, x2, x3] = (x0, x1, x2, x3)
-- to4Tuple = error "undefined"

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