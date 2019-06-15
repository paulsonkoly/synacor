module Main where

import IOSystem
import Machine
import System.Environment

main :: IO ()
main = do
  putStrLn "invoke with filename.bin argument"
  [filename] <- getArgs
  boot filename >>= runMachineIO
