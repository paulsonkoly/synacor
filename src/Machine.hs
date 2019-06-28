{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Machine
  ( Machine
  , ip
  , memory
  , stack
  , output
  , StepIn
  , mkStepIn
  , input
  , breaks
  , watches
  , traceST
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
import           Memory
import           Numeric
import qualified Data.Vector                   as V
import qualified Data.Set                      as Set
import           Lens.Micro
import           Lens.Micro.TH

data Machine = Machine
  { _ip     :: Word16
  , _memory :: Memory
  , _stack  :: [Word16]
  , _output :: String
  }

makeLenses ''Machine

mkMachine :: Machine
mkMachine = Machine 0 mkMemory [] []


data Attr = WordAttr | RegAttr

data StepOut
  = Break Word16          -- ^ breakpoint hit
  | Watch Word16          -- ^ watchpoint hit
  | Input                 -- ^ input required
  | Halt                  -- ^ halted
  | Continue              -- ^ normal operation
  | Invalid Word16 Word16 -- ^ invalid opcode


data OpCode = OpCode
  { _name       :: Text.Text
  , _attributes :: [Attr]
  , _microcode  :: [Word16] -> Machine -> StepIn -> (Machine, StepOut)
  }


data StepIn = StepIn
  { _input   :: Maybe Char
  , _breaks  :: Set.Set Word16
  , _watches :: Set.Set Word16
  , _traceST :: Bool
  }

makeLenses ''StepIn

mkStepIn :: StepIn
mkStepIn = StepIn Nothing Set.empty Set.empty False


machineOpMatrix :: V.Vector OpCode
machineOpMatrix = V.fromList
  [ OpCode "HALT" [] (\_ m _ -> (m, Halt))
  , OpCode
    "SET"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m _ -> (m & memory %~ writeWord attr0 attr1, Continue))
  , OpCode "PUSH" [RegAttr] (\[attr0] m _ -> (m & stack %~ (attr0 :), Continue))
  , OpCode
    "POP"
    [WordAttr]
    (\[attr0] m _ ->
      ( m & memory %~ writeWord attr0 (head $ m ^. stack) & stack %~ tail
      , Continue
      )
    )
  , OpCode
    "EQ"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = if attr1 == attr2 then 1 else 0
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "GT"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = if attr1 > attr2 then 1 else 0
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode "JMP" [RegAttr] (\[attr0] m _ -> (m & ip .~ attr0, Continue))
  , OpCode
    "JMPNZ"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m _ ->
      let jmp same = if attr0 /= 0 then attr1 else same
      in  (m & ip %~ jmp, Continue)
    )
  , OpCode
    "JMPZ"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m _ ->
      let jmp same = if attr0 == 0 then attr1 else same
      in  (m & ip %~ jmp, Continue)
    )
  , OpCode
    "ADD"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = (attr1 + attr2) .&. 0x7fff
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "MUL"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = (attr1 * attr2) .&. 0x7fff
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "MOD"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = (attr1 `mod` attr2) .&. 0x7fff
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "AND"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = attr1 .&. attr2
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "OR"
    [WordAttr, RegAttr, RegAttr]
    (\[attr0, attr1, attr2] m _ ->
      let value = attr1 .|. attr2
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "FLIP"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m _ ->
      let value = complement attr1 .&. 0x7fff
      in  (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "LD"
    [WordAttr, RegAttr]
    (\[attr0, attr1] m inp ->
      let value = readWord attr1 $ m ^. memory
      in  if attr1 `Set.member` (inp ^. watches)
            then (m, Watch attr1)
            else (m & memory %~ writeWord attr0 value, Continue)
    )
  , OpCode
    "ST"
    [RegAttr, RegAttr]
    (\[attr0, attr1] m inp ->
      let trace = if inp ^. traceST
            then showHex attr0 "" ++ " <- " ++ showHex attr1 "" ++ "\n"
            else ""
      in  (m & memory %~ writeWord attr0 attr1 & output .~ trace, Continue)
    )
  , OpCode
    "CALL"
    [RegAttr]
    (\[attr0] m _ -> (m & ip .~ attr0 & stack %~ (m ^. ip :), Continue))
  , OpCode
    "RET"
    []
    (\[] m _ -> case m ^. stack of
      []           -> (m, Halt)
      (x : stack') -> (m & ip .~ x & stack .~ stack', Continue)
    )
  , OpCode
    "OUT"
    [RegAttr]
    (\[attr0] m _ -> (m & output %~ (chr (fromIntegral attr0) :), Continue))
  , OpCode
    "IN"
    [WordAttr]
    (\[attr0] m inp -> case inp ^. input of
      Just c ->
        let value = fromIntegral $ ord c
        in  (m & memory %~ writeWord attr0 value, Continue)
      Nothing -> (m & ip -~ 2, Input)
    )
  , OpCode "NOOP" [] (\[] m _ -> (m, Continue))
  ]

step :: StepIn -> Machine -> (Machine, StepOut)
step inp m = if (m ^. ip) `Set.member` (inp ^. breaks)
  then (m, Break $ m ^. ip)
  else
    let instr = readWord (m ^. ip) (m ^. memory)
    in  if instr <= 21
          then
            let (OpCode _ attrs micro) = machineOpMatrix V.! fromIntegral instr
                attrs'                 = zipWith fetch [1, 2 ..] attrs
                attrs''                = map resolve attrs'
                hits                   = filter checkWatch attrs'
                m' = m & ip +~ fromIntegral (length attrs + 1)
            in  case hits of
                  []         -> micro attrs'' m' inp
                  (_, x) : _ -> (m, Watch x)
          else (m, Invalid (m ^. ip) instr)
 where
  fetch off WordAttr = (WordAttr, readWord (m ^. ip + off) $ m ^. memory)
  fetch off RegAttr  = (RegAttr, readWord (m ^. ip + off) $ m ^. memory)
  checkWatch (WordAttr, addr) = addr `Set.member` (inp ^. watches)
  checkWatch (RegAttr, value) =
    isRegister value && value `Set.member` (inp ^. watches)
  resolve (WordAttr, addr) = addr
  resolve (RegAttr, value) =
    if isRegister value then readWord value $ m ^. memory else value
  isRegister value = testBit value 15


disassCount :: Word16 -> Int -> Memory -> Text.Text
disassCount _ 0 _ = ""
disassCount addr n mem =
  let instr              = readWord addr mem
      (OpCode _ attrs _) = machineOpMatrix V.! fromIntegral instr
      size               = fromIntegral $ length attrs + 1
  in  if instr <= 21
        then
          Text.justifyLeft 8 ' ' (Text.pack (showHex addr ""))
          <> "| "
          <> disass addr mem
          <> "\n"
          <> disassCount (addr + size) (n - 1) mem
        else "DATA : " <> Text.intercalate " " (map transform [0 .. n - 1])
 where
  transform off =
    let value = readWord (addr + fromIntegral off) mem in dumpChar value


disass :: Word16 -> Memory -> Text.Text
disass addr mem =
  let instr = readWord addr mem
  in  if instr <= 21
        then
          let (OpCode name attrs _) = machineOpMatrix V.! fromIntegral instr
              attrs'                = zipWith resolve [1, 2 ..] attrs
          in  name <> " " <> Text.intercalate " " attrs'
        else "DATA : " <> dumpChar instr
 where
  resolve off RegAttr = "[" <> resolve off WordAttr <> "]"
  resolve off WordAttr =
    let r = readWord (addr + off) mem
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

