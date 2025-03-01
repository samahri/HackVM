{--
    Parse *.hack binary files into an executable.
    TODO: "Parser" may not be the right word for this operation
--}
module Nand2Tetris.HackParser (
    BinaryString
    , parseBinaryString
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
    a  <- parseZeroOrOne
    c0 <- parseZeroOrOne
    c1 <- parseZeroOrOne
    c2 <- parseZeroOrOne
    c3 <- parseZeroOrOne
    c4 <- parseZeroOrOne
    c5 <- parseZeroOrOne
    d0 <- parseZeroOrOne
    d1 <- parseZeroOrOne
    d2 <- parseZeroOrOne
    j0 <- parseZeroOrOne
    j1 <- parseZeroOrOne
    j2 <- parseZeroOrOne

    pure $ HackWord16F (One, One, One, a, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2)

parseAInstruction :: Parser HackWord16
parseAInstruction = do
    _ <- Megaparsec.char '0'
    n1 <- parseZeroOrOne
    n2 <- parseZeroOrOne
    n3 <- parseZeroOrOne
    n4 <- parseZeroOrOne
    n5 <- parseZeroOrOne
    n6 <- parseZeroOrOne
    n7 <- parseZeroOrOne
    n8 <- parseZeroOrOne
    n9 <- parseZeroOrOne
    n10 <- parseZeroOrOne
    n11 <- parseZeroOrOne
    n12 <- parseZeroOrOne
    n13 <- parseZeroOrOne
    n14 <- parseZeroOrOne
    n15 <- parseZeroOrOne
    
    pure $ HackWord16F (Zero, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)

parseZeroOrOne :: Parser Bit
parseZeroOrOne = choice [Zero <$ Megaparsec.char '0', One <$ Megaparsec.char '1']