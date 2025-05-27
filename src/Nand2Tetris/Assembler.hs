module Nand2Tetris.Assembler (
    assembleToBinaryCode
    , BinaryString
) where

import BasicPrelude hiding (read)
import Text.Read (read)
import Text.Megaparsec as Megaparsec hiding (State, label)
import Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Byte.Lexer as L
import Data.Void (Void)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State

import Nand2Tetris.HackParser

-- asm -> hack: [String] -> BinaryString -> File
-- running hack: File -> BinaryString -> [HackWord16] -> LoadMemory

type SymbolsMap = Map.Map String BinaryString

assembleToBinaryCode :: [String] -> IO BinaryString
assembleToBinaryCode input = do 
    print symbolsMap
    concat <$> mapM (assembleLine symbolsMap) input
    where
        symbolsMap :: SymbolsMap
        -- symbolsMap = mconcat [predefinedSymbolsMap, getLabelSymbolsMap input, getVarSymbolsMap input]
        -- foldl :: (StateT a -> StateT b -> StateT b) -> StateT a -> [StateT a] -> StateT b
        symbolsMap = execState (foldl (>>) (pure ()) [getLabelSymbolsMap input, getVarSymbolsMap input]) predefinedSymbolsMap

assembleLine :: SymbolsMap -> String -> IO BinaryString
assembleLine symbolsMap input = fromRight undefined <$> Megaparsec.runParserT (assemblyCodeParser symbolsMap) "" input

-- Parser Code
type Parser = ParsecT Void String IO

-- <line> = <commentLine> | <label> | <instruction>
assemblyCodeParser :: SymbolsMap -> Parser BinaryString
assemblyCodeParser symbolsMap = Megaparsec.space >> L.lexeme Megaparsec.space (parseCommentLine <|> parseLabelDeclaration symbolsMap <|> parseAssemblyInstruction symbolsMap)

parseAssemblyInstruction :: SymbolsMap -> Parser BinaryString
parseAssemblyInstruction symbolsMap = parseAInstruction symbolsMap <|> parseCInstruction

parseCommentLine :: Parser BinaryString
parseCommentLine = parseComment <|> parseEmptyLine

-- <commentLine> = ^\s*//[!-~]*\s*\n$
parseComment :: Parser BinaryString
parseComment = do
    _ <- Megaparsec.string "//"
    _ <- Megaparsec.many Megaparsec.printChar
    pure mempty

-- <label> = ^\s*([a-zA-Z]+)\s*<comment>.\n$
parseLabelDeclaration :: SymbolsMap -> Parser BinaryString
parseLabelDeclaration symbolsMap = do
    label <- Megaparsec.between (Megaparsec.char '(') (Megaparsec.char ')') (Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char '_'))
    _ <- Megaparsec.optional parseComment
    let labelMaybe = Map.lookup label symbolsMap
    maybe undefined pure labelMaybe

parseAInstruction :: SymbolsMap -> Parser BinaryString
parseAInstruction symbolsMap = Megaparsec.try (parseVariable symbolsMap) <|> parseConstant

parseConstant :: Parser BinaryString
parseConstant = do
    _ <- Megaparsec.char '@'
    num <- Megaparsec.some Megaparsec.digitChar
    -- todo: assert that num is 0–32767
    _ <- Megaparsec.optional parseComment
    pure (numToBinaryString (read num))

-- <variable> =  ^\s*@[a-zA-Z]+\s*\n^
parseVariable :: SymbolsMap -> Parser BinaryString
parseVariable symbolsMap = do
    _ <- Megaparsec.char '@'
    -- todo: only valid chars
    ch <- Megaparsec.letterChar
    label <- Megaparsec.many Megaparsec.alphaNumChar
    -- todo: assert that num is 0–32767
    _ <- Megaparsec.optional parseComment
    let addrMaybe = Map.lookup (ch:label) symbolsMap
    maybe undefined pure addrMaybe

{--
    111acccccdddjjj
    jjj: jump bits
    ddd: dest bits
    ccccc: command bits
--}
parseCInstruction :: Parser BinaryString
parseCInstruction = do
    (d0, d1, d2) <- parseDestination
    (a, c0, c1, c2, c3, c4, c5) <- parseComp 
    (j0, j1, j2) <- parseJump
    _ <- Megaparsec.optional parseComment
    pure ['1', '1', '1', a, c0, c1, c2, c3, c4, c5, d0, d1, d2, j0, j1, j2]

parseJump :: Parser (Char, Char, Char)
parseJump = Megaparsec.choice [parseNonNullJump, pure ('0', '0', '0')]
    where
        parseNonNullJump :: Parser (Char, Char, Char)
        parseNonNullJump =  Megaparsec.char ';' >> Megaparsec.choice [
                ('0', '0', '1') <$ Megaparsec.string "JGT",
                ('0', '1', '0') <$ Megaparsec.string "JEQ",
                ('0', '1', '1') <$ Megaparsec.string "JGE",
                ('1', '0', '0') <$ Megaparsec.string "JLT",
                ('1', '0', '1') <$ Megaparsec.string "JNE",
                ('1', '1', '1') <$ Megaparsec.string "JMP"
                ]

parseDestination :: Parser (Char, Char, Char)
parseDestination = Megaparsec.try parseNonNullDest <|> pure ('0', '0', '0')
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
        opChars = Megaparsec.choice $ Megaparsec.char <$> ['+', '-', '&', '|']

parseEmptyLine :: Parser BinaryString
parseEmptyLine = mempty <$ Megaparsec.eof

numToBinaryString :: Int -> BinaryString
numToBinaryString = padZeros . toBinaryString . toBinary
    where
        toBinaryString :: [Int] -> BinaryString
        toBinaryString = concatMap show

        toBinary :: Int -> [Int]
        toBinary 0 = [ 0 ]
        toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

        padZeros :: BinaryString -> BinaryString
        padZeros bs = replicate (16 - length bs) '0' <> bs

-- Symbols Map
predefinedSymbolsMap :: SymbolsMap
predefinedSymbolsMap = Map.fromList [
    ("R0",  "0000000000000000"),
    ("R1",  "0000000000000001"),
    ("R2",  "0000000000000010"),
    ("R3",  "0000000000000011"),
    ("R4",  "0000000000000100"),
    ("R5",  "0000000000000101"),
    ("R6",  "0000000000000110"),
    ("R7",  "0000000000000111"),
    ("R8",  "0000000000001000"),
    ("R9",  "0000000000001001"),
    ("R10", "0000000000001010"),
    ("R11", "0000000000001011"),
    ("R12", "0000000000001100"),
    ("R13", "0000000000001101"),
    ("R14", "0000000000001110"),
    ("R15", "0000000000001111"),
    ("END", "1111111111111111")
    ]

-- type LabelParser = ParsecT Void Strin(Writer SymbolsMap)
type LabelParser = Parsec Void String (SymbolsMap, Int)

type SymbolParserState = State SymbolsMap ()

getLabelSymbolsMap :: [String] -> SymbolParserState
-- foldM :: (Foldable t, Monad m) => (Int -> String -> m b) -> Int -> [String] -> m b
-- getLabelSymbolsMap = mconcat . fmap parseAndCreateMap 
getLabelSymbolsMap input = do
    symbolsMap <- get
    let newMap = fst $ foldl func (symbolsMap, 0) input
    put newMap
    where
        func :: (SymbolsMap, Int) -> String -> (SymbolsMap, Int)
        func (sm, n) str = case eitherToMaybe $ runParser (encodeLabelSymbol sm n) "" str of
            Just (smap, n') -> (smap, n')
            Nothing -> undefined

encodeLabelSymbol :: SymbolsMap -> Int -> LabelParser
encodeLabelSymbol symbolsMap n = Megaparsec.space >> Megaparsec.choice [
    do 
       _ <- parseComment' <|> parseEmptyLine'
       pure (symbolsMap, n)
    , do
        -- todo: only valid chars
        label <- Megaparsec.between (Megaparsec.char '(') (Megaparsec.char ')') (Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char '_'))
        _ <- Megaparsec.optional parseComment'
        case Map.lookup label symbolsMap of
            Nothing -> pure (Map.insert label (numToBinaryString $ n + 1) symbolsMap, n + 1)
            Just _ -> pure (symbolsMap, n + 1)
    , do
        _ <- Megaparsec.manyTill Megaparsec.anySingle Megaparsec.eof
        pure (symbolsMap, n + 1)
    ]

getVarSymbolsMap :: [String] -> SymbolParserState
getVarSymbolsMap input = do
    symbolsMap <- get
    let newMap = fst $ foldl func (symbolsMap, 15) input
    put newMap
    where
        func :: (SymbolsMap, Int) -> String -> (SymbolsMap, Int)
        func (sm, n) str = case eitherToMaybe $ runParser (encodeVarSymbol sm n) "" str of
            Just (smap, n') -> (smap, n')
            Nothing -> undefined
    
encodeVarSymbol :: SymbolsMap -> Int -> LabelParser
encodeVarSymbol symbolsMap n = Megaparsec.space >> Megaparsec.choice [
    Megaparsec.try $ do
        _ <- Megaparsec.char '@'
        -- todo: only valid chars
        ch <- Megaparsec.letterChar
        label <- Megaparsec.many Megaparsec.alphaNumChar
        -- todo: assert that num is 0–32767
        _ <- Megaparsec.optional parseComment'
        case Map.lookup (ch:label) symbolsMap of
            Nothing -> pure (Map.insert (ch:label) (numToBinaryString $ n + 1) symbolsMap, n + 1)
            Just _ -> pure (symbolsMap, n + 1)
    , do
       _ <- Megaparsec.manyTill Megaparsec.anySingle Megaparsec.eof
       pure (symbolsMap, n)
    ]

-- parseVariable :: SymbolsMap -> Parser BinaryString
-- parseVariable symbolsMap = do
--     _ <- Megaparsec.char '@'
--     -- todo: only valid chars
--     ch <- Megaparsec.letterChar
--     label <- Megaparsec.many Megaparsec.printChar
--     -- todo: assert that num is 0–32767
--     _ <- Megaparsec.optional parseComment
--     let addrMaybe = Map.lookup (ch:label) symbolsMap
--     maybe undefined pure addrMaybe

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

parseComment' :: Parsec Void String ()
parseComment' = do
    _ <- Megaparsec.string "//"
    _ <- Megaparsec.manyTill Megaparsec.printChar Megaparsec.eof
    pure ()

parseEmptyLine' :: Parsec Void String ()
parseEmptyLine' = () <$ (Megaparsec.space >> Megaparsec.eof)