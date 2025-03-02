{--
    Parse *.hack binary files into an executable.
    TODO: "Parser" may not be the right word for this operation
--}
{-# LANGUAGE LambdaCase #-}
module Nand2Tetris.HackParser (
    BinaryString
    , bytecodeToHackAssem
) where

import Nand2Tetris.Types.Bit
import Nand2Tetris.Types.HackWord16

import BasicPrelude
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char as Megaparsec
import Data.Void (Void)

type Parser = Parsec Void BinaryString
-- newtype BinaryString = BinaryString String deriving Show
type BinaryString = String

bytecodeToHackAssem :: BinaryString -> [HackWord16]
bytecodeToHackAssem binaryString = fromMaybe undefined (parseBinaryString binaryString)

parseBinaryString :: BinaryString -> Maybe [HackWord16]
parseBinaryString = Megaparsec.parseMaybe hackCodeParser

hackCodeParser :: Parser [HackWord16]
hackCodeParser = Megaparsec.manyTill (Megaparsec.space >> parseInstruction) eof

parseInstruction :: Parser HackWord16
parseInstruction = parseAInstruction <|> parseCInstruction

parseCInstruction :: Parser HackWord16
parseCInstruction = do
    _ <- Megaparsec.char '1'
    _ <- Megaparsec.char '1'
    _ <- Megaparsec.char '1'
    a  <- Megaparsec.binDigitChar
    c0 <- Megaparsec.binDigitChar
    c1 <- Megaparsec.binDigitChar
    c2 <- Megaparsec.binDigitChar
    c3 <- Megaparsec.binDigitChar
    c4 <- Megaparsec.binDigitChar
    c5 <- Megaparsec.binDigitChar
    d0 <- Megaparsec.binDigitChar
    d1 <- Megaparsec.binDigitChar
    d2 <- Megaparsec.binDigitChar
    j0 <- Megaparsec.binDigitChar
    j1 <- Megaparsec.binDigitChar
    j2 <- Megaparsec.binDigitChar

    pure $ toHackBit <$> HackWord16F ('1', '1', '1', a, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2)

parseAInstruction :: Parser HackWord16
parseAInstruction = do
    _ <- Megaparsec.char '0'
    n1 <- Megaparsec.binDigitChar
    n2 <- Megaparsec.binDigitChar
    n3 <- Megaparsec.binDigitChar
    n4 <- Megaparsec.binDigitChar
    n5 <- Megaparsec.binDigitChar
    n6 <- Megaparsec.binDigitChar
    n7 <- Megaparsec.binDigitChar
    n8 <- Megaparsec.binDigitChar
    n9 <- Megaparsec.binDigitChar
    n10 <- Megaparsec.binDigitChar
    n11 <- Megaparsec.binDigitChar
    n12 <- Megaparsec.binDigitChar
    n13 <- Megaparsec.binDigitChar
    n14 <- Megaparsec.binDigitChar
    n15 <- Megaparsec.binDigitChar
    
    pure $ toHackBit <$> HackWord16F ('0', n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)

toHackBit :: Char -> Bit
toHackBit = \case
    '0' -> Zero
    '1' -> One
    _ -> undefined