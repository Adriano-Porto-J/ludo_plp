module Main where

import Interface.InterfaceDebug
import Interface.Rendering

debug = False 

main :: IO ()
main = do
  if debug
    then initGameTerminal
    else render