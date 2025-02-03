module Nand2Tetris.Memory (
    DFF
   ,DFF16
   ,dff
   ,bit
   ,register
   ,ram8
--    ,ram64
--    ,ram512
--    ,ram4K
--    ,ram16K
   ,pc
) where

import Nand2Tetris.Types.Bit(Bit(..))
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Gates (mux, mux4Way16, mux16, dMux8Way, dMux8Way16)
import Nand2Tetris.Chips (inc16)
import BasicPrelude (length, (==), ($), pure, (<$>), replicate, fst, fmap, (&&), (<>), Int, (^), (+), foldr, (.), undefined)
import Control.Exception (assert)
import Control.Monad.Trans.State.Strict (State, get, put, runState, execState)
import Data.List (zipWith, (!!), splitAt, unzip)

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
type InitialState = MemoryState

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
type Ram8State = [MemoryState] -- length is 8 word16

type RAMOutput = State Ram8State Output16

ram8 :: Input16 -> Address -> Load -> RAMOutput
ram8 input16 addr load = do
    ramState <- get
    let inputList = tuple8ToList $ dMux8Way16 input16 addr
        loadArr = tuple8ToList $ dMux8Way load addr
        registerList = zipWith register inputList loadArr
        nextCycleOutput = zipWith execState registerList ramState
    put nextCycleOutput
    pure $ mux8Way16 ramState addr

tuple8ToList :: (a, a, a, a, a, a, a, a) -> [a]
tuple8ToList (x0, x1, x2, x3, x4, x5, x6, x7) = [x0, x1, x2, x3, x4, x5, x6, x7] 

-- mux4Way16 :: (Input16, Input16, Input16, Input16) -> (Sel, Sel) -> Output16
mux8Way16 :: [Input16] -> Address -> Output16
mux8Way16 registerList (addr0, addr1, addr2) = assert (length registerList == 8) $ mux16 (mux4Way16 registers0to3 addressTuple, mux4Way16 registers4to7 addressTuple) addr0
    where
        addressTuple = (addr1, addr2)
        (registers0to3, registers4to7) = let (r0to3List, r3to7l) = splitAt 4 registerList in (to4Tuple r0to3List, to4Tuple r3to7l)

        to4Tuple :: [Input16] -> (Input16, Input16, Input16, Input16)
        to4Tuple [b0, b1, b2, b3] = (b0, b1, b2, b3)
            
-- convertToInt :: [Bit] -> Int
-- convertToInt input = fst (foldr func (0, 0) input)
--     where
--         func :: Bit -> (Int, Int) -> (Int, Int)
--         func b (total, acc) = if b == Zero then (total, acc + 1) else (total + 2^acc, acc + 1)

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