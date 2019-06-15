module IOSystem
  ( runMachineIO
  , boot
  )
where

import           Numeric
import Data.Word
import qualified Data.Text                     as T
import           Data.List
import           Memory
import           System.Console.Readline
import           Machine
import           Control.Monad

import Debug.Trace

boot :: FilePath -> IO Machine
boot fp = do
  mem <- loadMemory fp
  return $ mkMachine { memory = mem }


runMachineIO :: Machine -> IO ()
runMachineIO = go "" [0]
 where
  go :: String -> [Word16] -> Machine -> IO ()
  go input breaks m@(Machine ip memory stack out)
    | ip `elem` breaks = do
      putStrLn $ "breakpoint " ++ showHex ip "" ++ " hit"
      ioLoop input breaks m
    | readWord ip memory == 20 = case input of
      ""     -> ioLoop input breaks m
      c : cs -> go cs breaks (fst $ step c m)
    | otherwise = case step '\0' m of
      (_ , False) -> return ()
      (m', True ) -> do
        putStr $ reverse $ output m'
        go input breaks (m' { output = [] })
  ioLoop input breaks m@(Machine ip memory stack out) = do
    mbLine <- readline "% "
    case mbLine of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just "regs" -> do
        putStrLn $ "ip : " ++ showHex ip ""
        forM_ [0 .. 7] $ \i -> putStrLn $ "r" ++ show i ++ ": " ++ showHex
          (readWord (0x8000 + i) memory)
          ""
        ioLoop input breaks m
      Just "stack" -> do
        forM_ stack $ putStrLn . flip showHex ""
        ioLoop input breaks m
      Just "breaks" -> do
        forM_ breaks $ putStrLn . flip showHex ""
        ioLoop input breaks m
      Just "cont" -> go input breaks (fst $ step '\n' m)
      Just "step" -> do
        putStrLn $ T.unpack $ disassCount ip 1 memory
        ioLoop input breaks (fst $ step '\n' m)
      Just line
        | "disass" `isPrefixOf` line
        -> let [_, addr, count] = words line
           in  do
                 case (readHex addr, reads count) of
                   ([(addr', "")], [(count', "")]) ->
                     putStrLn $ T.unpack $ disassCount addr' count' memory
                   _ -> putStrLn "invalid input"
                 ioLoop input breaks m
        | "set" `isPrefixOf` line
        -> let [_, addr, value] = words line
           in  do
                 case (readHex addr, readHex value) of
                   ([(addr', "")], [(value', "")]) ->
                    ioLoop input breaks m { memory = writeWord addr' value' memory }
                   _ -> putStrLn "invalid input"
                 ioLoop input breaks m
        | "break" `isPrefixOf` line
        -> one_arg line (\addr -> ioLoop input (addr : breaks) m) input breaks m
        | "delete" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop input (delete addr breaks) m)
                   input
                   breaks
                   m
        | "jump" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop input breaks m { ip = addr })
                   input
                   breaks
                   m
        | otherwise
        -> go (line ++ "\n") breaks m
  one_arg line f input breaks m =
    let [_, addr] = words line
    in  case readHex addr of
          [(addr', "")] -> f addr'
          _             -> do
            putStrLn "invalid input"
            ioLoop input breaks m
