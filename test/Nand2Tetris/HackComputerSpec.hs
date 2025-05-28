module Nand2Tetris.HackComputerSpec (
    spec
) where

import Test.Hspec

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Gates (mux16)
import Nand2Tetris.Chips (inc16, alu, AluCtrl(..))
import Nand2Tetris.Memory (rom32K)
import Nand2Tetris.TestUtil
import Nand2Tetris.HackComputer

import BasicPrelude
import Control.Monad.Trans.State.Strict (execState, evalState, execStateT, evalStateT)
import Test.QuickCheck 

spec :: Spec
spec = do
    context "cpu" $ do
        it "resets the CPU when reset flag is enabled" $
            property $ \initialARegister initialDRegister initialPc mInput (c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2) arand ->
                let
                    instruction = HackWord16F (One, One, One, arand, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2)
                    initialState = (initialARegister, initialDRegister, initialPc)
                    (_, _, pcReg) = execState (cpu mInput instruction One) initialState
                in
                    pcReg === zeros

        it "Writes the content of A Register to writeM" $
            property $ \initialARegister initialDRegister initialPc mInput (c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2) arand ->
                let
                    instruction = HackWord16F (One, One, One, arand, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2)
                    initialState = (initialARegister, initialDRegister, initialPc)
                    (mAddress, _, _, _) = evalState (cpu mInput instruction One) initialState
                in
                    mAddress === initialARegister

        it "implements an A-instruction" $
            forAll genAInstruction $ \aInstruction -> property $ \initialARegister initialDRegister initialPc mInput ->
                let
                    initialState = (initialARegister, initialDRegister, initialPc)
                    nextInstrAddr = inc16 initialPc
                    (aReg, dReg, pcReg) = execState (cpu mInput aInstruction Zero) initialState
                in
                    -- Set the A-Register to @xxx AND increments to the next instruction AND D register remain unchanged
                    aReg === aInstruction .&&. pcReg === nextInstrAddr .&&. dReg === initialDRegister

        context "implements a C-instruction" $ do
            it "Stores comp in..." $ withMaxSuccess 1000 $
                forAll genRandomAluCtrl $ \(a, randomAluCtrl) -> property $ \initialARegister initialDRegister initialPc mInput j0 j1 j2 d0 d1 d2 ->
                    classify ((d0, d1, d2) == (Zero, Zero, Zero)) "NULL" $
                    classify ((d0, d1, d2) == (Zero, Zero, One)) "M" $
                    classify ((d0, d1, d2) == (Zero, One, Zero)) "D" $
                    classify ((d0, d1, d2) == (Zero, One, One)) "DM" $
                    classify ((d0, d1, d2) == (One, Zero, Zero)) "A" $
                    classify ((d0, d1, d2) == (One, Zero, One)) "AM" $
                    classify ((d0, d1, d2) == (One, One, Zero)) "AD" $
                    classify ((d0, d1, d2) == (One, One, One)) "ADM" $
                        let
                            instruction = HackWord16F (One, One, One, a,
                                            zx randomAluCtrl, nx randomAluCtrl, zy randomAluCtrl, ny randomAluCtrl, f randomAluCtrl, no randomAluCtrl ,
                                            d0, d1, d2,
                                            j0, j1, j2)
                            initialState = (initialARegister, initialDRegister, initialPc)
                            (aluOutput, _, _) = alu (initialDRegister, mux16 (initialARegister, mInput) a) randomAluCtrl
                            (_, mOutput, mwrite, _) = evalState (cpu mInput instruction Zero) initialState
                            (aregister, dregister, _) = execState (cpu mInput instruction Zero) initialState
                        in
                            mwrite === d2
                            .&&.
                            mOutput === aluOutput
                            .&&.
                            (if d1 == One
                                then dregister === mOutput
                                else dregister === initialDRegister) -- "equals mOutput when d1d; unchanged when d0d"
                            .&&.
                            (if d0 == One
                                then aregister === mOutput
                                else aregister === initialARegister)

            it "implements jumps" $ withMaxSuccess 1000 $
                    forAll genRandomAluCtrl $ \(a, aluCtrl) -> property $ \initialARegister initialDRegister initialPc mInput j0 j1 j2 d0 d1 d2 ->
                        classify ((j0, j1, j2) == (Zero, Zero, Zero)) "NULL" $
                        classify ((j0, j1, j2) == (Zero, Zero, One)) "JGT" $
                        classify ((j0, j1, j2) == (Zero, One, Zero)) "JEQ" $
                        classify ((j0, j1, j2) == (Zero, One, One)) "JGE" $
                        classify ((j0, j1, j2) == (One, Zero, Zero)) "JTL" $
                        classify ((j0, j1, j2) == (One, Zero, One)) "JNE" $
                        classify ((j0, j1, j2) == (One, One, Zero)) "JLE" $
                        classify ((j0, j1, j2) == (One, One, One)) "JMP" $
                        let
                            instruction = HackWord16F (One, One, One, a,
                                    zx aluCtrl, nx aluCtrl, zy aluCtrl, ny aluCtrl, f aluCtrl, no aluCtrl ,
                                    d0, d1, d2,
                                    j0, j1, j2)
                            initialState = (initialARegister, initialDRegister, initialPc)
                            (_, _, pcReg) = execState (cpu mInput instruction Zero) initialState
                            (_, mOutput, _, _) = evalState (cpu mInput instruction Zero) initialState
                            zr = if mOutput == zeros then One else Zero
                            ng = head (toList mOutput)
                            nextInstrAddr = inc16 initialPc
                        in
                            if (j0 == One && ng == One && zr == Zero) || (j1 == One && ng == Zero && zr == One) || (j2 == One && ng == Zero && zr == Zero)
                                then collect ("Jump " ++ show j0 ++ show j1 ++ show j2) $ pcReg === initialARegister
                                else collect ("No Jump " ++ show j0 ++ show j1 ++ show j2) $ pcReg === nextInstrAddr 

    context "mainMemory" $ do
        it "reads from the ram address space" pending

    context "hackComputer" $ do
        {--
            forall (computer :: Reset -> HackComputer, 
                    rom :: ROMAddress -> ROM32kOutput, 
                    x :: HackWord16,
                    r :: Reset).
                rom 0x_000 0000 0000 0000 = x -> cpuInstruction (execState (computer 1 >> computer r)) == x
        --}
        it "ROM points to address zero when computer is reset" $ do

            initialCpuInstruction <- random16Bits
            cpuInput <- random16Bits
            memoryState <- random32KMemory
            screenState <- randomRam16K
            rom32kState <- random32KMemory

            initialDRegister <- random16Bits
            initialARegister <- random16Bits
            initialPc <- random16Bits

            bit <- randomBit

            -- type ComputerState = (MemoryState, Ram16kState, ROM32kState, CpuRegisters, CpuInstruction, CpuInput)
            let initialState = (memoryState, screenState, rom32kState, (initialARegister, initialDRegister, initialPc), initialCpuInstruction, cpuInput)
                address0 = pure Zero :: HackWord16
                romValAtAddr0 = evalState (rom32K address0) rom32kState

            (_, _, _, _, cpuInstruction, _) <- execStateT (hackComputer One >> hackComputer bit) initialState

            cpuInstruction `shouldBe` romValAtAddr0

        {--
            forall (computer :: Reset -> HackComputer,
                    mainMemory :: MemoryAddress -> MainMemory
                    addressM :: MemoryAddress,
                    x :: HackWord16 ).
                mainMemory addressM = x -> cpuInput (execState (computer 0)) == x
        --}
        it "cpu input = memory[addressM]" $ do

            cpuInstruction <- random16Bits
            initialCpuInput <- random16Bits
            memoryState <- random32KMemory
            ram16kState <- randomRam16K
            rom32kState <- random32KMemory

            addressM <- toHackWord16 . replaceHead . toList <$> random16Bits

            randomInput <- random16Bits
            loadBit <- randomBit

            initialDRegister <- random16Bits
            let initialARegister = addressM
            initialPc <- random16Bits

            let initialState = (memoryState, ram16kState, rom32kState, (initialARegister, initialDRegister, initialPc), cpuInstruction, initialCpuInput)

            memoryOutput <- mainMemory addressM randomInput loadBit `evalStateT` (memoryState, ram16kState)

            (_, _, _, _, _, cpuInput) <- execStateT (hackComputer Zero) initialState

            cpuInput `shouldBe` memoryOutput


replaceHead :: [Bit] -> [Bit]
replaceHead [] = []
replaceHead (_:xs) = reverse (Zero : xs)




