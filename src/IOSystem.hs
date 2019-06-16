module IOSystem
  ( runMachineIO
  , boot
  )
where

import           Numeric
import qualified Data.Set                      as Set
import           Data.Word
import qualified Data.Text                     as T
import           Data.List
import           Memory
import           System.Console.Readline
import           Machine
import           Control.Monad

boot :: FilePath -> IO Machine
boot fp = do
  mem <- loadMemory fp
  return $ mkMachine { memory = mem }


runMachineIO :: Machine -> IO ()
runMachineIO = go "" (StepIn Nothing (Set.singleton 0) Set.empty)
 where
  go :: String -> StepIn -> Machine -> IO ()
  go iString sin m = 
    let sin' = sin { input = Nothing }
    in case step sin m of
      (m', Continue) -> do
        putStr $ reverse $ output m'
        go iString sin' (m' { output = [] })
      (_ , Halt ) -> return ()
      (m', Input) -> case iString of
        ""     ->
          ioLoop "" sin' m
        c : cs -> go cs sin { input = Just c } m
      (_, Break ip) -> do
        putStrLn $ "breakpoint " ++ showHex ip "" ++ " hit"
        ioLoop iString sin' m
      (_, Watch addr) -> do
        putStrLn $ "watchpoint " ++ showHex addr "" ++ " hit"
        ioLoop iString sin' m
      (_, Invalid ip opcode) -> do
        putStrLn $ "invalid opcode " ++ showHex ip "" ++ " " ++ showHex opcode ""
        ioLoop iString sin' m
  ioLoop iString sin m@(Machine ip memory stack out) = do
    mbLine <- readline "% "
    case mbLine of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just "regs" -> do
        putStrLn $ "ip : " ++ showHex ip ""
        forM_ [0 .. 7] $ \i -> putStrLn $ "r" ++ show i ++ ": " ++ showHex
          (readWord (0x8000 + i) memory)
          ""
        ioLoop iString sin m
      Just "stack" -> do
        forM_ stack $ putStrLn . flip showHex ""
        ioLoop iString sin m
      Just "breaks" -> do
        forM_ (breaks sin) $ putStrLn . flip showHex ""
        ioLoop iString sin m
      Just "watches" -> do
        forM_ (watches sin) $ putStrLn . flip showHex ""
        ioLoop iString sin m
      Just "cont" -> 
        case step (sin { breaks = Set.empty, watches = Set.empty }) m of
          (m', Continue) -> go iString sin m'
          (m', _)        -> ioLoop iString sin m'
      Just "step" -> do
        putStrLn $ T.unpack $ disassCount ip 1 memory
        case step (sin { breaks = Set.empty, watches = Set.empty }) m of
          (m', Continue) -> ioLoop iString sin m'
          (m', _       ) -> go iString sin m
      Just line
        | "disass" `isPrefixOf` line
        -> let [_, addr, count] = words line
           in  do
                 case (readHex addr, reads count) of
                   ([(addr', "")], [(count', "")]) ->
                     putStrLn $ T.unpack $ disassCount addr' count' memory
                   _ -> putStrLn "invalid input"
                 ioLoop iString sin m
        | "set" `isPrefixOf` line
        -> let [_, addr, value] = words line
           in  do
                 case (readHex addr, readHex value) of
                   ([(addr', "")], [(value', "")]) -> ioLoop
                     iString
                     sin
                     m { memory = writeWord addr' value' memory }
                   _ -> putStrLn "invalid input"
                 ioLoop iString sin m
        | "break" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString (addBreak addr sin) m)
                   iString
                   sin
                   m
        | "delete" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString (delBreak addr sin) m)
                   iString
                   sin
                   m
        | "watch" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString (addWatch addr sin) m)
                   iString
                   sin
                   m
        | "delwatch" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString (delWatch addr sin) m)
                   iString
                   sin
                   m
        | "jump" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString sin m { ip = addr })
                   iString
                   sin
                   m
        | otherwise
        -> go (line ++ "\n") sin m
  one_arg line f iString sin m =
    let [_, addr] = words line
    in  case readHex addr of
          [(addr', "")] -> f addr'
          _             -> do
            putStrLn "invalid input"
            ioLoop iString sin m
