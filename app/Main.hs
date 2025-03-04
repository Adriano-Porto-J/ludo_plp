module Main where

import Interface.InterfaceDebug
import Interface.Rendering

debug = True 

main :: IO ()
main = do
  if debug
    then initGameTerminal
    else render