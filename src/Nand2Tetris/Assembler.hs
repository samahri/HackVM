module Nand2Tetris.Assembler (
    convertToHackAssem
    ,hackAssemToByteCode
) where

import BasicPrelude
import Data.ByteString as BS
import Data.Binary (encode)

import Nand2Tetris.Types.HackWord16
import Nand2Tetris.Types.Bit


convertToHackAssem :: [String] -> [HackWord16]
convertToHackAssem _ = [pure Zero]

hackAssemToByteCode :: [HackWord16] -> ByteString
hackAssemToByteCode hackwordList = BS.concat $ encodeStrictBytestring <$> hackwordList
    where
        encodeStrictBytestring :: HackWord16 -> ByteString
        encodeStrictBytestring = BS.toStrict . encode

-- Convert a Word8 to an 8-character binary string
-- word8ToBinary :: Word8 -> String
-- word8ToBinary w = [if testBit w i then '1' else '0' | i <- [7,6..0]]

-- -- Convert a ByteString to a binary string
-- byteStringToBinary :: B.ByteString -> String
-- byteStringToBinary = concatMap word8ToBinary . B.unpack