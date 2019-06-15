{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Machine
  ( Machine(..)
  , mkMachine
  , (<-|)
  , step
  , disass
  , disassCount
  )
where

import           Data.Word
import           Data.Bits
import           Data.Char
import qualified Data.Text                     as T
import           Control.Monad.State
import           Memory
import           Data.Tuple
import           Numeric
import qualified Data.Vector                   as V

data Machine = Machine
  { ip     :: Word16
  , memory :: Memory
  , stack  :: [Word16]
  , output :: String
  }


mkMachine :: Machine
mkMachine = Machine 0 mkMemory [] []


data Attr = WordAttr | RegAttr


machineOpMatrix
  :: V.Vector (T.Text, [Attr], [Word16] -> Machine -> Char -> (Machine, Bool))
machineOpMatrix = V.fromList
  [ ("HALT", [], \attrs m input -> (m, False))
  , ( "SET"
    , [WordAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      (m { memory = writeWord attr0 attr1 memory }, True)
    )
  , ( "PUSH"
    , [RegAttr]
    , \[attr0] m@(Machine ip memory stack out) input ->
      (m { stack = attr0 : stack }, True)
    )
  , ( "POP"
    , [WordAttr]
    , \[attr0] m@(Machine ip memory stack out) input ->
      ( m { memory = writeWord attr0 (head stack) memory, stack = tail stack }
      , True
      )
    )
  , ( "EQ"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = if attr1 == attr2 then 1 else 0
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "GT"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = if attr1 > attr2 then 1 else 0
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "JMP"
    , [RegAttr]
    , \[attr0] m@(Machine ip memory stack out) input -> (m { ip = attr0 }, True)
    )
  , ( "JMPNZ"
    , [RegAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      let ip' = if attr0 /= 0 then attr1 else ip in (m { ip = ip' }, True)
    )
  , ( "JMPZ"
    , [RegAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      let ip' = if attr0 == 0 then attr1 else ip in (m { ip = ip' }, True)
    )
  , ( "ADD"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = (attr1 + attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "MUL"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = (attr1 * attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "MOD"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = (attr1 `mod` attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "AND"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = attr1 .&. attr2
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "OR"
    , [WordAttr, RegAttr, RegAttr]
    , \[attr0, attr1, attr2] m@(Machine ip memory stack out) input ->
      let value = attr1 .|. attr2
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "FLIP"
    , [WordAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      let value = complement attr1 .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "LD"
    , [WordAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      let value = readWord attr1 memory
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ( "ST"
    , [RegAttr, RegAttr]
    , \[attr0, attr1] m@(Machine ip memory stack out) input ->
      (m { memory = writeWord attr0 attr1 memory }, True)
    )
  , ( "CALL"
    , [RegAttr]
    , \[attr0] m@(Machine ip memory stack out) input ->
      (m { ip = attr0, stack = ip : stack }, True)
    )
  , ( "RET"
    , []
    , \[] m@(Machine ip memory stack out) input -> case stack of
      []           -> (m, False)
      (x : stack') -> (m { ip = x, stack = stack' }, True)
    )
  , ( "OUT"
    , [RegAttr]
    , \[attr0] m@(Machine ip memory stack out) input ->
      (m { output = chr (fromIntegral attr0) : out }, True)
    )
  , ( "IN"
    , [WordAttr]
    , \[attr0] m@(Machine ip memory stack out) input ->
      let value = fromIntegral $ ord input
      in  (m { memory = writeWord attr0 value memory }, True)
    )
  , ("NOOP", [], \[] m@(Machine ip memory stack out) input -> (m, True))
  ]


(<-|) :: Machine -> (Word16, Word16) -> Machine
(<-|) m (addr, value) = m { memory = writeWord addr value (memory m) }

infixl 9 <-|



step :: Char -> Machine -> (Machine, Bool)
step input m@(Machine ip memory stack out) =
  let instr = readWord ip memory
  in  if instr <= 21
        then
          let (_, attrs, micro) = machineOpMatrix V.! fromIntegral instr
              attrs' = zipWith resolve [1, 2 ..] attrs
              m' = m { ip = ip + fromIntegral (length attrs) + 1 }
          in  micro attrs' m' input
        else error $ "unexpected op code : " ++ showHex instr ""
 where
  resolve ix WordAttr = readWord (ip + ix) memory
  resolve ix RegAttr  = readReg (ip + ix) memory


disassCount :: Word16 -> Int -> Memory -> T.Text
disassCount addr 0 memory = ""
disassCount addr n memory =
  let instr         = readWord addr memory
      (_, attrs, _) = machineOpMatrix V.! fromIntegral instr
      size          = fromIntegral $ length attrs + 1
  in  if instr <= 21
        then
          T.justifyLeft 8 ' ' (T.pack (showHex addr ""))
          <> "| "
          <> disass addr memory
          <> "\n"
          <> disassCount (addr + size) (n - 1) memory
        else "DATA : " <> T.intercalate " " (map transform [0 .. n - 1])
 where
  transform ix =
    let value = readWord (addr + fromIntegral ix) memory in dumpChar value


disass :: Word16 -> Memory -> T.Text
disass addr memory =
  let instr = readWord addr memory
  in  if instr <= 21
        then
          let (name, attrs, _) = machineOpMatrix V.! fromIntegral instr
              attrs'           = zipWith resolve [1, 2 ..] attrs
          in  name <> " " <> T.intercalate " " attrs'
        else "DATA : " <> dumpChar instr
 where
  resolve ix RegAttr = "[" <> resolve ix WordAttr <> "]"
  resolve ix WordAttr =
    let r = readWord (addr + ix) memory
    in  if 0x8000 <= r && r <= 0x8007
          then "r" <> T.pack (show $ r - 0x8000)
          else T.pack (showHex r "")


dumpChar :: Word16 -> T.Text
dumpChar value
  | (65 <= value && 90 <= value) || (97 <= value && 122 <= value)
  = T.pack (showHex value "")
    <> "("
    <> T.singleton (chr (fromIntegral value))
    <> ")"
  | otherwise
  = T.pack (showHex value "")

