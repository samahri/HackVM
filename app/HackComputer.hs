module Main (main) where

import BasicPrelude
import qualified CLI.HackComputer (main)

main :: IO ()
main = CLI.HackComputer.main