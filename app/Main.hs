module Main where

import InterfaceDebug

debug = True

main :: IO ()
main = do
  if debug
    then initGameTerminal
    else putStrLn "Not implemented..."