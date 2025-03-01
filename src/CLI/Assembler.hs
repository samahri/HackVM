{-# LANGUAGE OverloadedStrings #-}
module CLI.Assembler (
    main
) where

import BasicPrelude
import qualified System.IO as IO
import System.Exit

import CLI.Utils
import Nand2Tetris.Assembler

{--
    reads a command line argument, file.asm, of Hack assembly language, converts it to a binary
    file, writes the resulting bytecode to file.hack
--}
main :: IO ()
main = do
    (asmFileEither, hackFileEither) <- getFileNames
    
    assemCode <- case asmFileEither of
        Right asmFile -> readAsmContent asmFile
        Left _ -> putStrLn "no file exists" >> exitFailure
    
    let hackFile = fromEither hackFileEither
    
    createHackFile hackFile assemCode
    where
        fromEither :: Either String String -> String
        fromEither hackFileEither = case hackFileEither of
            Right hackFile -> hackFile
            Left hackFile -> hackFile

createHackFile :: String -> [String] -> IO ()
-- TODO: use a handle
createHackFile filepath hackAssem =  IO.writeFile filepath (hackAssemToByteCode . convertToHackAssem $ hackAssem)

readAsmContent :: FilePath -> IO [String]
readAsmContent asmFile = reverse <$> IO.withFile asmFile IO.ReadMode (readContent [] (:))