module Memory
  ( Memory
  , mkMemory
  , readWord
  , writeWord
  , readReg
  , loadMemory
  )
where

import qualified Data.IntMap.Strict            as M
import           Data.Maybe
import qualified Data.Vector.Unboxed           as V
import           Data.Word
import           Data.Bits
import qualified Data.ByteString               as BS

type Chunk = V.Vector Word16


chunkSize :: Int
chunkSize = 128


mkChunk :: Chunk
mkChunk = V.replicate chunkSize 0


newtype Memory = Memory (M.IntMap Chunk)


mkMemory :: Memory
mkMemory = Memory M.empty


readWord :: Word16 -> Memory -> Word16
readWord addr (Memory mem) =
  let (chunkAddr, chunkOff) = splitAddr addr
  in  case M.lookup chunkAddr mem of
        Just chunk -> chunk V.! chunkOff
        Nothing    -> 0


readReg :: Word16 -> Memory -> Word16
readReg addr mem =
  let value = readWord addr mem
  in if testBit value 15 then readWord value mem else value


writeWord :: Word16 -> Word16 -> Memory -> Memory
writeWord addr value (Memory mem) =
  let (chunkAddr, chunkOff) = splitAddr addr
      chunk                 = fromMaybe mkChunk (M.lookup chunkAddr mem)
      chunk'                = chunk `V.unsafeUpd` [(chunkOff, value)]
  in  Memory $ M.insert chunkAddr chunk' mem


splitAddr :: Word16 -> (Int, Int)
splitAddr addr = fromIntegral addr `divMod` chunkSize



writeHalfWord :: Word16 -> Int -> Word8 -> Memory -> Memory
writeHalfWord addr bit value memory =
  let w    = readWord addr memory
      mask = if bit /= 0 then 0xff else 0xff00
      w'   = (w .&. mask) .|. (fromIntegral value `shiftL` (8 * bit))
  in  writeWord addr w' memory


loadMemory :: FilePath -> IO Memory
loadMemory fn = do
  bs <- BS.readFile fn
  return $ (\(_, _, m) -> m) $ BS.foldl'
    (\(addr, bit, m) byte ->
      ( addr + fromIntegral bit
      , 1 - bit
      , writeHalfWord addr bit byte m
      )
    )
    (0, 0, mkMemory)
    bs
