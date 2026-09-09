{-# LANGUAGE OverloadedStrings #-}
module CLI.Assembler (
    main
) where

import BasicPrelude
import qualified System.IO as IO
import System.Exit(die)

import CLI.Utils
import Nand2Tetris.Assembler

{--
    reads a command line argument, file.asm, of Hack assembly language, converts it to a binary
    file, writes the resulting bytecode to file.hack
--}
main :: IO ()
main = do
    (asmFileEither, hackFileEither) <- getFileNames
    
    asmFile <- either
        (die . ("no file exists" ++))
        pure
        asmFileEither
    
    let hackFile = either id id hackFileEither

    hackMachineCode <- (assemblyToBinaryCode <=< readAssemblyFile) asmFile
    createHackFile hackFile hackMachineCode
    where
        readAssemblyFile :: FilePath -> IO [String]
        readAssemblyFile filePath = reverse <$> IO.withFile filePath IO.ReadMode (readContent [] (:))

        createHackFile = IO.writeFile 