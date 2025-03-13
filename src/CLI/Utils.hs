module CLI.Utils (
    readContent
    , readContent'
    ,getFileNames
) where

import Options.Applicative 
import System.IO as IO
import System.Directory
import BasicPrelude
import qualified Data.List as L

readContent' :: String -> Handle -> IO String
readContent' acc handle = do
    line <- IO.hGetLine handle
    isReady <- IO.hIsEOF handle
    if isReady 
        then pure (acc <> line)
        else readContent' (acc <> line) handle

readContent :: a -> (String -> a -> a) -> Handle -> IO a
readContent acc fn handle = do
    line <- IO.hGetLine handle
    isReady <- IO.hIsEOF handle
    if isReady 
        then pure (fn line acc)
        else readContent (fn line acc) fn handle

type AssemblyFile = String
type BinaryFile = String
getFileNames :: IO (Either AssemblyFile AssemblyFile, Either BinaryFile BinaryFile)
getFileNames = 
    let
        addExtentions file = (file <> ".asm", file <> ".hack")
    in do
        (asmFile, hackFile) <- addExtentions <$> execParser hackAssemblyParserInfo
        asmExists <- doesFileExist asmFile 
        hackExists <- doesFileExist hackFile
        pure (toEither asmExists asmFile, toEither hackExists hackFile)
    where
        toEither :: Bool -> a -> Either a a
        toEither exists x = if exists then Right x else Left x

hackAssemblyParserInfo :: ParserInfo String
hackAssemblyParserInfo = info (hackAssemblyParser <**> helper) (progDesc "yada" <> header "Hack Assembler Program")

-- TODO: The file name may contain a file path. If no path is specified, the assembler operates on the current folder.
hackAssemblyParser :: Parser String
hackAssemblyParser = argument (removeExt <$> str) (metavar "file" <> help "Hack Assembly file")

removeExt :: String -> String
removeExt file = 
    if ".asm" `L.isSuffixOf` file 
        then L.takeWhile (/= '.') file -- TODO: what if there's multiple suffixes
        else file 