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
    
    assemblyCode <- case asmFileEither of
        Right asmFile -> readAssemblyFile asmFile
        Left _ -> putStrLn "no file exists" >> exitFailure
    
    let hackFile = fromEither hackFileEither

    hackMachineCode <- assembleToBinaryCode assemblyCode
    createHackFile hackFile hackMachineCode
    where
        fromEither :: Either String String -> String
        fromEither hackFileEither = case hackFileEither of
            Right hackFile -> hackFile
            Left hackFile -> hackFile
        createHackFile :: String -> BinaryString -> IO ()
        -- TODO: use a handle
        createHackFile = IO.writeFile 

readAssemblyFile :: FilePath -> IO [String]
readAssemblyFile filePath = reverse <$> IO.withFile filePath IO.ReadMode (readContent [] (:))