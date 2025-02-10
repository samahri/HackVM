module Nand2Tetris.InputOutputSpec (
    spec
) where

import Nand2Tetris.Types.Bit(Bit(One, Zero))
import Nand2Tetris.TestUtil
import Nand2Tetris.InputOutput
import Control.Monad.Trans.State.Strict (evalState)
import BasicPrelude (($), (>>), IO, pure, (==), mod, (+))
-- import Nand2Tetris.Memory
import Test.Hspec
import Control.Monad (replicateM_)

spec :: Spec
spec = do
    context "Screen" $ do
        let get14BitAddress = do
                addrBit0 <- randomBit
                addrBit1 <- randomBit
                addrBit2 <- randomBit
                addrBit3 <- randomBit
                addrBit4 <- randomBit
                addrBit5 <- randomBit
                addrBit6 <- randomBit
                addrBit7 <- randomBit
                addrBit8 <- randomBit
                addrBit9 <- randomBit
                addrBit10 <- randomBit
                addrBit11 <- randomBit
                addrBit12 <- randomBit
                addrBit13 <- randomBit

                if 
                    (addrBit0, addrBit1, addrBit2, addrBit3, addrBit4, addrBit5, addrBit6, addrBit7, addrBit8, addrBit9, addrBit10, addrBit11, addrBit12, addrBit13) 
                    == (Zero, One, Zero, Zero, One, Zero, Zero, One, Zero, Zero, One, Zero, One, Zero)
                    then get14BitAddress 
                    else pure (addrBit0, addrBit1, addrBit2, addrBit3, addrBit4, addrBit5, addrBit6, addrBit7, addrBit8, addrBit9, addrBit10, addrBit11, addrBit12, addrBit13)
       
        it "writes and reads an 16-bit number" $ replicateM_ 10 $ do
            inputbits1 <- random16Bits
            inputbits2 <- random16Bits
            inputbits3 <- random16Bits

            initialBits <- randomRam16K
            
            addr1 <- get14BitAddress
            addr2 <- get14BitAddress

            bits0 <- random16Bits

            evalState (screen addr1 inputbits1 One  >> screen addr2 inputbits2 One  >> screen (Zero, One, Zero, Zero, One, Zero, Zero, One, Zero, Zero, One, Zero, One, Zero) inputbits3 One  >> screen addr2 bits0 Zero) initialBits `shouldBe` inputbits2
    
    it "Keyboard" pending