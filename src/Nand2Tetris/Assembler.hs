module Nand2Tetris.Assembler (
    compileHack
    , BinaryString
) where

import BasicPrelude hiding (read)
import Text.Read (read)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char as Megaparsec
import Data.Void (Void)

import Nand2Tetris.HackParser

-- asm -> hack: [String] -> BinaryString -> File
-- running hack: File -> BinaryString -> [HackWord16] -> LoadMemory

compileHack :: [String] -> IO BinaryString
compileHack input = fmap concat (mapM encodeBinaryString input)

encodeBinaryString :: String -> IO BinaryString
encodeBinaryString input = do 
    parserResult <- Megaparsec.runParserT assemblyParser "" input
    case parserResult of
        Right bs -> pure bs
        Left _ -> undefined

type Parser = ParsecT Void String IO

assemblyParser :: Parser BinaryString
assemblyParser = Megaparsec.space >> (parseComments <|> parseAInstruction <|> parseCInstruction <|> parseEmptyLine)

parseComments :: Parser BinaryString
parseComments = do
    _ <- Megaparsec.string "//"
    _ <- Megaparsec.manyTill Megaparsec.printChar Megaparsec.eof
    pure mempty

parseAInstruction :: Parser BinaryString
parseAInstruction = do
    _ <- Megaparsec.char '@'
    num <- Megaparsec.some Megaparsec.digitChar
    _ <- Megaparsec.space
    _ <- Megaparsec.optional parseComments
    _ <- Megaparsec.eof
    pure (numToBinaryString num)

parseCInstruction :: Parser BinaryString
parseCInstruction = do
    _ <- Megaparsec.space
    (d0, d1, d2) <- parseDestination
    _ <- Megaparsec.space
    (a, c0, c1, c2, c3, c4, c5) <- parseComp 
    _ <- Megaparsec.space
    (j0, j1, j2) <- parseJump
    _ <- Megaparsec.space
    _ <- Megaparsec.optional parseComments
    _ <- Megaparsec.eof
    pure ['1', '1', '1', a, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2]

parseJump :: Parser (Char, Char, Char)
parseJump = Megaparsec.choice [parseNonNullJump, pure ('0', '0', '0')]
    where
        parseNonNullJump :: Parser (Char, Char, Char)
        parseNonNullJump =  Megaparsec.choice [
                ('0', '0', '1') <$ Megaparsec.string "JGT",
                ('0', '1', '0') <$ Megaparsec.string "JEQ",
                ('0', '1', '1') <$ Megaparsec.string "JGE",
                ('1', '0', '0') <$ Megaparsec.string "JLT",
                ('1', '0', '1') <$ Megaparsec.string "JNE",
                ('1', '1', '1') <$ Megaparsec.string "JMP"
                ]

parseDestination :: Parser (Char, Char, Char)
parseDestination = Megaparsec.choice [parseNonNullDest, pure ('0', '0', '0')]
    where
        parseNonNullDest :: Parser (Char, Char, Char)
        parseNonNullDest = do 
            (d0, d1, d2) <- Megaparsec.choice [
                    ('0', '0', '1') <$ Megaparsec.char 'M',
                    ('0', '1', '0') <$ Megaparsec.char 'D',
                    ('0', '1', '1') <$ Megaparsec.string "DM",
                    ('1', '0', '0') <$ Megaparsec.char 'A',
                    ('1', '0', '1') <$ Megaparsec.string "AM",
                    ('1', '1', '0') <$ Megaparsec.string "AD",
                    ('1', '1', '1') <$ Megaparsec.string "ADM"
                ]
            _ <- Megaparsec.space
            _ <- Megaparsec.char '='
            pure (d0, d1, d2)

parseComp :: Parser (Char, Char, Char, Char, Char, Char, Char)
parseComp = Megaparsec.choice [
        ('0', '1', '0', '1', '0', '1', '0') <$ Megaparsec.char '0',
        ('0', '1', '1', '1', '1', '1', '1') <$ Megaparsec.char '1',
        ('0', '1', '1', '1', '0', '1', '0') <$ Megaparsec.string "-1",
        ('0', '0', '0', '1', '1', '0', '0') <$ Megaparsec.try (Megaparsec.char 'D' >> Megaparsec.notFollowedBy opChars),
        ('0', '1', '1', '0', '0', '0', '0') <$ Megaparsec.try (Megaparsec.char 'A' >> Megaparsec.notFollowedBy opChars),
        ('1', '1', '1', '0', '0', '0', '0') <$ Megaparsec.try (Megaparsec.char 'M' >> Megaparsec.notFollowedBy opChars),
        ('0', '0', '0', '1', '1', '0', '1') <$ Megaparsec.string "!D",
        ('0', '1', '1', '0', '0', '0', '1') <$ Megaparsec.string "!A",
        ('1', '1', '1', '0', '0', '0', '1') <$ Megaparsec.string "!M",
        ('0', '0', '0', '1', '1', '1', '1') <$ Megaparsec.string "-D",
        ('0', '1', '1', '0', '0', '1', '1') <$ Megaparsec.string "-A",
        ('1', '1', '1', '0', '0', '1', '1') <$ Megaparsec.string "-M",
        ('0', '0', '1', '1', '1', '1', '1') <$ Megaparsec.string "D+1",
        ('0', '1', '1', '0', '1', '1', '1') <$ Megaparsec.string "A+1",
        ('1', '1', '1', '0', '1', '1', '1') <$ Megaparsec.string "M+1",
        ('0', '0', '0', '1', '1', '1', '0') <$ Megaparsec.string "D-1",
        ('0', '1', '1', '0', '0', '1', '0') <$ Megaparsec.string "A-1",
        ('1', '1', '1', '0', '0', '1', '0') <$ Megaparsec.string "M-1",
        ('0', '0', '0', '0', '0', '1', '0') <$ Megaparsec.string "D+A",
        ('1', '0', '0', '0', '0', '1', '0') <$ Megaparsec.string "D+M",
        ('0', '0', '1', '0', '0', '1', '1') <$ Megaparsec.string "D-A",
        ('1', '0', '1', '0', '0', '1', '1') <$ Megaparsec.string "D-M",
        ('0', '0', '0', '0', '1', '1', '1') <$ Megaparsec.string "A-D",
        ('1', '0', '0', '0', '1', '1', '1') <$ Megaparsec.string "M-D",
        ('0', '0', '0', '0', '0', '0', '0') <$ Megaparsec.string "D&A",
        ('1', '0', '0', '0', '0', '0', '0') <$ Megaparsec.string "D&M",
        ('0', '0', '1', '0', '1', '0', '1') <$ Megaparsec.string "D|A",
        ('1', '0', '1', '0', '1', '0', '1') <$ Megaparsec.string "D|M"
    ]
    where
        opChars :: Parser Char
        opChars = Megaparsec.char '+' <|> Megaparsec.char '-' <|> Megaparsec.char '&' <|> Megaparsec.char '|'

parseEmptyLine :: Parser BinaryString
parseEmptyLine = mempty <$ (Megaparsec.space >> Megaparsec.eof)

numToBinaryString :: String -> BinaryString
numToBinaryString = padZeros . toBinaryString . toBinary . toInt
    where
        toInt :: String -> Int
        toInt = read

        toBinaryString :: [Int] -> BinaryString
        toBinaryString = concatMap show

        toBinary :: Int -> [Int]
        toBinary 0 = [ 0 ]
        toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

        padZeros :: BinaryString -> BinaryString
        padZeros bs = replicate (16 - length bs) '0' <> bs