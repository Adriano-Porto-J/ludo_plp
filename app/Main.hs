module Main where

import Interface.InterfaceDebug

debug = True

main :: IO ()
main = do
  if debug
    then initGameTerminal
    else putStrLn "Not implemented..."