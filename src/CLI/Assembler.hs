{-# LANGUAGE OverloadedStrings #-}
module CLI.Assembler (
    main
    ,getFileNames
) where

import BasicPrelude
import qualified Data.List as L
import Options.Applicative 
import qualified System.IO as IO
import System.Directory
import System.Exit
import Data.ByteString as BS

import Nand2Tetris.Assembler

type AssemblyFile = String
type BinaryFile = String

{--
    reads a command line argument, file.asm, of Hack assembly language, converts it to a binary
    file, writes the resulting bytecode to file.hack
--}
main :: IO ()
main = do
    (asmFileEither, hackFileEither) <- getFileNames
    
    -- exists <- doesFileExist asmFile 
    assemCode <- case asmFileEither of
        Right asmFile -> readContent asmFile
        Left _ -> putStrLn "no file exists" >> exitFailure
    
    hackFile <- case hackFileEither of
        Right hackFile -> pure hackFile
        Left hackFile -> pure hackFile
    
    createHackFile hackFile assemCode

createHackFile :: String -> [String] -> IO ()
-- TODO: use a handle
createHackFile filepath hackAssem =  BS.writeFile filepath (hackAssemToByteCode . convertToHackAssem $ hackAssem)

-- TODO: maybe move it to separate module
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
        
        

-- TODO: maybe move it to separate module
readContent :: FilePath -> IO [String]
readContent asmFile = L.reverse <$> IO.withFile asmFile IO.ReadMode (readContent' [])
    where
        readContent' :: [String] -> IO.Handle -> IO [String]
        readContent' strArr handle = do
            line <- IO.hGetLine handle
            isReady <- IO.hIsEOF handle
            if isReady 
                then pure (line : strArr)
                else readContent' (line : strArr) handle


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