{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Machine
  ( Machine(..)
  , StepIn(..)
  , addBreak
  , delBreak
  , addWatch
  , delWatch
  , StepOut(..)
  , mkMachine
  , step
  , disass
  , disassCount
  )
where

import           Data.Word
import           Data.Bits
import           Data.Char
import qualified Data.Text                     as Text
import           Control.Monad.State
import           Memory
import           Data.Tuple
import           Numeric
import qualified Data.Vector                   as V
import qualified Data.Set                      as Set

data Machine = Machine
  { ip     :: Word16
  , memory :: Memory
  , stack  :: [Word16]
  , output :: String
  }


mkMachine :: Machine
mkMachine = Machine 0 mkMemory [] []


data Attr = WordAttr | RegAttr


data OpCode = OpCode
  { name       :: Text.Text
  , attributes :: [Attr]
  , microcode  :: [Word16] -> Machine -> StepIn -> (Machine, StepOut)
  }


data StepIn = StepIn
  { input   :: Maybe Char
  , breaks  :: Set.Set Word16
  , watches :: Set.Set Word16
  }


addBreak :: Word16 -> StepIn -> StepIn
addBreak addr (StepIn input breaks watches) =
  StepIn input (Set.insert addr breaks) watches


delBreak :: Word16 -> StepIn -> StepIn
delBreak addr (StepIn input breaks watches) =
  StepIn input (Set.delete addr breaks) watches


addWatch :: Word16 -> StepIn -> StepIn
addWatch addr (StepIn input breaks watches) =
  StepIn input breaks (Set.insert addr watches)


delWatch :: Word16 -> StepIn -> StepIn
delWatch addr (StepIn input breaks watches) =
  StepIn input breaks (Set.delete addr watches)


data StepOut
  = Break Word16          -- ^ breakpoint hit
  | Watch Word16          -- ^ watchpoint hit
  | Input                 -- ^ input required
  | Halt                  -- ^ halted
  | Continue              -- ^ normal operation
  | Invalid Word16 Word16 -- ^ invalid opcode


machineOpMatrix :: V.Vector OpCode
machineOpMatrix = V.fromList
  [ OpCode "HALT" [] (\_ m _ -> (m, Halt))
  , OpCode
    "SET"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) _ ->
      (m { memory = writeWord attr0 attr1 memory }, Continue)
    )
  , OpCode
    "PUSH"
    [RegAttr]
    (\[attr0] m@(Machine ip memory stack out) _ ->
      (m { stack = attr0 : stack }, Continue)
    )
  , OpCode
    "POP"
    [WordAttr]
    (\[attr0] m@(Machine ip memory stack out) _ ->
      ( m { memory = writeWord attr0 (head stack) memory, stack = tail stack }
      , Continue
      )
    )
  , OpCode
    "EQ"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = if attr1 == attr2 then 1 else 0
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "GT"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = if attr1 > attr2 then 1 else 0
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "JMP"
    [RegAttr]
    (\[attr0] m@(Machine ip memory stack out) _ -> (m { ip = attr0 }, Continue))
  , OpCode
    "JMPNZ"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) _ ->
      let ip' = if attr0 /= 0 then attr1 else ip in (m { ip = ip' }, Continue)
    )
  , OpCode
    "JMPZ"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) _ ->
      let ip' = if attr0 == 0 then attr1 else ip in (m { ip = ip' }, Continue)
    )
  , OpCode
    "ADD"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = (attr1 + attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "MUL"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = (attr1 * attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "MOD"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = (attr1 `mod` attr2) .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "AND"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = attr1 .&. attr2
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "OR"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m@(Machine ip memory stack out) _ ->
      let value = attr1 .|. attr2
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "FLIP"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) _ ->
      let value = complement attr1 .&. 0x7fff
      in  (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "LD"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) sin ->
      let value = readWord attr1 memory
      in  if attr1 `Set.member` watches sin
            then (m, Watch attr1)
            else (m { memory = writeWord attr0 value memory }, Continue)
    )
  , OpCode
    "ST"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m@(Machine ip memory stack out) _ ->
      (m { memory = writeWord attr0 attr1 memory }, Continue)
    )
  , OpCode
    "CALL"
    [RegAttr]
    (\[attr0] m@(Machine ip memory stack out) _ ->
      (m { ip = attr0, stack = ip : stack }, Continue)
    )
  , OpCode
    "RET"
    []
    (\[] m@(Machine ip memory stack out) _ -> case stack of
      []           -> (m, Halt)
      (x : stack') -> (m { ip = x, stack = stack' }, Continue)
    )
  , OpCode
    "OUT"
    [RegAttr]
    (\[attr0] m@(Machine ip memory stack out) _ ->
      (m { output = chr (fromIntegral attr0) : out }, Continue)
    )
  , OpCode
    "IN"
    [WordAttr]
    (\[attr0] m@(Machine ip memory stack out) sin -> case input sin of
      Just c ->
        let value = fromIntegral $ ord c
        in  (m { memory = writeWord attr0 value memory }, Continue)
      Nothing -> (m { ip = ip - 2 }, Input)
    )
  , OpCode "NOOP" [] (\[] m@(Machine ip memory stack out) _ -> (m, Continue))
  ]

step :: StepIn -> Machine -> (Machine, StepOut)
step sin@(StepIn mbInput breaks watches) m@(Machine ip memory stack out) =
  if ip `Set.member` breaks
    then (m, Break ip)
    else
      let instr = readWord ip memory
      in  if instr <= 21
            then
              let (OpCode _ attrs micro) =
                    machineOpMatrix V.! fromIntegral instr
                  attrs'  = zipWith fetch [1, 2 ..] attrs
                  attrs'' = map resolve attrs'
                  hits    = filter checkWatch attrs'
                  m'      = m { ip = ip + fromIntegral (length attrs) + 1 }
              in  case hits of
                    []         -> micro attrs'' m' sin
                    (_, x) : _ -> (m, Watch x)
            else (m, Invalid ip instr)
 where
  fetch ix WordAttr = (WordAttr, readWord (ip + ix) memory)
  fetch ix RegAttr  = (RegAttr, readWord (ip + ix) memory)
  checkWatch (WordAttr, addr ) = addr `Set.member` watches
  checkWatch (RegAttr , value) = isRegister value && value `Set.member` watches
  resolve (WordAttr, addr) = addr
  resolve (RegAttr, value) =
    if isRegister value then readWord value memory else value
  isRegister value = testBit value 15


disassCount :: Word16 -> Int -> Memory -> Text.Text
disassCount addr 0 memory = ""
disassCount addr n memory =
  let instr              = readWord addr memory
      (OpCode _ attrs _) = machineOpMatrix V.! fromIntegral instr
      size               = fromIntegral $ length attrs + 1
  in  if instr <= 21
        then
          Text.justifyLeft 8 ' ' (Text.pack (showHex addr ""))
          <> "| "
          <> disass addr memory
          <> "\n"
          <> disassCount (addr + size) (n - 1) memory
        else "DATA : " <> Text.intercalate " " (map transform [0 .. n - 1])
 where
  transform ix =
    let value = readWord (addr + fromIntegral ix) memory in dumpChar value


disass :: Word16 -> Memory -> Text.Text
disass addr memory =
  let instr = readWord addr memory
  in  if instr <= 21
        then
          let (OpCode name attrs _) = machineOpMatrix V.! fromIntegral instr
              attrs'                = zipWith resolve [1, 2 ..] attrs
          in  name <> " " <> Text.intercalate " " attrs'
        else "DATA : " <> dumpChar instr
 where
  resolve ix RegAttr = "[" <> resolve ix WordAttr <> "]"
  resolve ix WordAttr =
    let r = readWord (addr + ix) memory
    in  if 0x8000 <= r && r <= 0x8007
          then "r" <> Text.pack (show $ r - 0x8000)
          else Text.pack (showHex r "")


dumpChar :: Word16 -> Text.Text
dumpChar value
  | (65 <= value && 90 <= value) || (97 <= value && 122 <= value)
  = Text.pack (showHex value "")
    <> "("
    <> Text.singleton (chr (fromIntegral value))
    <> ")"
  | otherwise
  = Text.pack (showHex value "")

