module Nand2Tetris.Assembler (
    convertToHackAssem
    ,hackAssemToByteCode
    ,bytecodeToHackAssem
    , BinaryString
) where

import BasicPrelude

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit
import Nand2Tetris.HackParser

-- asm -> hack: [String] -> [HackWord16] -> BinaryString -> File
-- running hack: File -> BinaryString -> [HackWord16] -> LoadMemory

convertToHackAssem :: [String] -> [HackWord16]
convertToHackAssem _ = [pure Zero]

hackAssemToByteCode :: [HackWord16] -> BinaryString
hackAssemToByteCode = concatMap encodeBinaryString

encodeBinaryString :: HackWord16 -> BinaryString
encodeBinaryString = undefined -- core logic of the assembler goes here

bytecodeToHackAssem :: BinaryString -> [HackWord16]
bytecodeToHackAssem binaryString = fromMaybe undefined (parseBinaryString binaryString)