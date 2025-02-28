{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

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

main :: IO ()
main = do
    (asmFile, hackFile) <- getFileNames
    
    exists <- doesFileExist asmFile 
    unless exists $ putStrLn "no file exists" >> exitFailure
    
    assemCode <- readContent asmFile
    
    createHackFile hackFile assemCode

createHackFile :: String -> [String] -> IO ()
-- TODO: use a handle
createHackFile filepath hackAssem =  BS.writeFile filepath (hackAssemToByteCode . convertToHackAssem $  hackAssem)

getFileNames :: IO (AssemblyFile, BinaryFile)
getFileNames = 
    let
        addExtentions file = (file <> ".asm", file <> ".hack")
    in
        addExtentions <$> execParser hackAssemblyParserInfo

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

