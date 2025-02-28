module Main(main) where

import qualified CLI.Assembler (main)
import BasicPrelude

main :: IO ()
main = CLI.Assembler.main

