module IOSystem
  ( runMachineIO
  , boot
  )
where

import           Numeric
import qualified Data.Set                      as Set
import           Data.Char
import qualified Data.Text                     as T
import           Data.List
import           Memory
import           System.Console.Readline
import           Machine
import           Control.Monad
import           Lens.Micro

boot :: FilePath -> IO Machine
boot fp = do
  mem <- loadMemory fp
  return $ mkMachine & memory .~ mem


runMachineIO :: Machine -> IO ()
runMachineIO = go "" (mkStepIn & breaks %~ Set.insert 0)
 where
  go :: String -> StepIn -> Machine -> IO ()
  go iString inp m
    = let inp' = inp & input .~ Nothing
      in
        case step inp m of
          (m', Continue) -> do
            putStr $ m' ^. output
            go iString inp' (m' & output .~ [])
          (_, Halt ) -> return ()
          (_, Input) -> case iString of
            ""     -> ioLoop "" inp' m
            c : cs -> go cs (inp & input ?~ c) m
          (_, Break ip') -> do
            putStrLn $ "breakpoint " ++ showHex ip' "" ++ " hit"
            ioLoop iString inp' m
          (_, Watch addr) -> do
            putStrLn $ "watchpoint " ++ showHex addr "" ++ " hit"
            ioLoop iString inp' m
          (_, Invalid ip' opcode) -> do
            putStrLn $ "invalid opcode " ++ showHex ip' "" ++ " " ++ showHex
              opcode
              ""
            ioLoop iString inp' m
  ioLoop :: String -> StepIn -> Machine -> IO ()
  ioLoop iString inp m = do
    mbLine <- readline "% "
    case mbLine of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just "regs" -> do
        putStrLn $ "ip : " ++ showHex (m ^. ip) ""
        forM_ [0 .. 7] $ \i -> putStrLn $ "r" ++ show i ++ ": " ++ showHex
          (readWord (0x8000 + i) $ m ^. memory)
          ""
        ioLoop iString inp m
      Just "stack" -> do
        forM_ (m ^. stack) $ putStrLn . flip showHex ""
        ioLoop iString inp m
      Just "breaks" -> do
        forM_ (inp ^. breaks) $ putStrLn . flip showHex ""
        ioLoop iString inp m
      Just "watches" -> do
        forM_ (inp ^. watches) $ putStrLn . flip showHex ""
        ioLoop iString inp m
      Just "cont" ->
        case step (inp & breaks .~ Set.empty & watches .~ Set.empty) m of
          (m', Continue) -> go iString inp m'
          (m', _       ) -> ioLoop iString inp m'
      Just "step" -> do
        putStrLn $ T.unpack $ disassCount (m ^. ip) 1 (m ^. memory)
        case step (inp & breaks .~ Set.empty & watches .~ Set.empty) m of
          (m', Continue) -> ioLoop iString inp m'
          (_ , _       ) -> go iString inp m
      Just "tracest" ->
        let trace' = not $ inp ^. traceST
        in  do
              putStrLn $ "tracing stores is " ++ if trace' then "on" else "off"
              ioLoop iString (inp & traceST .~ trace') m
      Just line
        | "disass" `isPrefixOf` line
        -> case words line of
          [_, addr, count] -> do
            case (readHex addr, reads count) of
              ([(addr', "")], [(count', "")]) ->
                putStrLn $ T.unpack $ disassCount addr' count' $ m ^. memory
              _ -> putStrLn "invalid input"
            ioLoop iString inp m
          _ -> do
            putStrLn "invalid input"
            ioLoop iString inp m
        | "string" `isPrefixOf` line
        -> case words line of
          [_, addr, count] -> do
            case (readHex addr, reads count) of
              ([(addr', "")], [(count', "")]) -> putStrLn
                [ chr $ fromIntegral $ readWord off $ m ^. memory
                | off <- [addr' .. addr' + count' - 1]
                ]
              _ -> putStrLn "invalid input"
            ioLoop iString inp m
          _ -> do
            putStrLn "invalid input"
            ioLoop iString inp m
        | "set" `isPrefixOf` line
        -> let [_, addr, value] = words line
           in  do
                 case (readHex addr, readHex value) of
                   ([(addr', "")], [(value', "")]) ->
                     ioLoop iString inp (m & memory %~ writeWord addr' value')
                   _ -> putStrLn "invalid input"
                 ioLoop iString inp m
        | "break" `isPrefixOf` line
        -> one_arg
          line
          (\addr -> ioLoop iString (inp & (breaks %~ Set.insert addr)) m)
          iString
          inp
          m
        | "delete" `isPrefixOf` line
        -> one_arg
          line
          (\addr -> ioLoop iString (inp & breaks %~ Set.delete addr) m)
          iString
          inp
          m
        | "watch" `isPrefixOf` line
        -> one_arg
          line
          (\addr -> ioLoop iString (inp & watches %~ Set.insert addr) m)
          iString
          inp
          m
        | "delwatch" `isPrefixOf` line
        -> one_arg
          line
          (\addr -> ioLoop iString (inp & watches %~ Set.delete addr) m)
          iString
          inp
          m
        | "jump" `isPrefixOf` line
        -> one_arg line
                   (\addr -> ioLoop iString inp (m & ip .~ addr))
                   iString
                   inp
                   m
        | otherwise
        -> go (line ++ "\n") inp m
  one_arg line f iString inp m =
    let [_, addr] = words line
    in  case readHex addr of
          [(addr', "")] -> f addr'
          _             -> do
            putStrLn "invalid input"
            ioLoop iString inp m
