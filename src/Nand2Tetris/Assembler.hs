module Nand2Tetris.Assembler (
    convertToHackAssem
    ,hackAssemToByteCode
    ,bytecodeToHackAssem
) where

import BasicPrelude
-- import Data.ByteString as BS
-- import Data.Binary (encode)

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit

-- asm -> hack: [String] -> [HackWord16] -> BinaryString -> File
-- running hack: File -> BinaryString -> [HackWord16] -> LoadMemory

newtype BinaryString = BinaryString String deriving Show

convertToHackAssem :: [String] -> [HackWord16]
convertToHackAssem _ = [pure Zero]

hackAssemToByteCode :: [HackWord16] -> BinaryString
hackAssemToByteCode = concatMap encodeBinaryString

encodeBinaryString :: HackWord16 -> BinaryString
encodeBinaryString = undefined -- core logic of the assembler goes here

bytecodeToHackAssem :: BinaryString -> [HackWord16]
bytecodeToHackAssem _ = [
        HackWord16F (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One, One),
        HackWord16F (One, One, One, Zero, One, One, Zero, Zero, Zero, Zero, Zero, One, Zero, Zero, Zero, Zero),
        HackWord16F (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One, One),
        HackWord16F (One, One, One, Zero, Zero, Zero, Zero, Zero, One, Zero, Zero, One, Zero, Zero, Zero, Zero),
        HackWord16F (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero),
        HackWord16F (One, One, One, Zero, Zero, Zero, One, One, Zero, Zero, Zero, Zero, One, Zero, Zero, Zero)
    ] 

-- Convert a Word8 to an 8-character binary string
-- word8ToBinary :: Word8 -> String
-- word8ToBinary w = [if testBit w i then '1' else '0' | i <- [7,6..0]]

-- -- Convert a ByteString to a binary string
-- byteStringToBinary :: B.ByteString -> String
-- byteStringToBinary = concatMap word8ToBinary . B.unpack