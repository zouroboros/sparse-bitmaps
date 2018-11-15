module Encodings where

import Control.Monad
import Data.Word
import qualified Data.Vector as V
import Text.Printf
import Data.List

import SparseMatrix

import Debug.Trace

type CompressedMatrix = (Int, Int, V.Vector Word8, V.Vector Word8)

compressV :: V.Vector Int -> V.Vector Word8
compressV vec = let
  shifts shift v = (v - shift) `div` 255
  enc _ v     | v < 255         = [fromIntegral v]
  enc shift v | v - shift < 255 = [fromIntegral (v - shift)]
  enc shift v                   = replicate (shifts shift v) 255 ++ [fromIntegral (v - (shifts shift v * 255))]
  ( _, list) = foldl (\(shift, accum) v -> (shifts shift v, accum ++ enc shift v)) (0, []) (V.toList vec)
  in V.fromList list

-- | Encodes a sparse bool matrix as two byte arrays
compressed :: SparseBoolMatrix -> CompressedMatrix
compressed (rs, cols, rowColumns) = let
  cColumns = V.map (compressV . (V.map (\x -> x - 1))) rowColumns
  cSizes = V.map V.length cColumns
  rps = V.scanl1' (+) cSizes
  crps = V.cons 0 (compressV rps)
  in (rs, cols, join cColumns, V.take (V.length crps - 1) crps)

hexArray :: V.Vector Word8 -> String
hexArray v = intercalate "," (map (printf "%#x") (V.toList v))

cPlusPlusStruct :: CompressedMatrix -> String -> String
cPlusPlusArray (rows, columns, cids, rps) name = let
  cidName = name ++ "_column_indices"
  rpName = name ++ "_row_pointer" in concat
    ["uint8_t ", cidName, " [] = { ", hexArray cids, " };\n",
      "uint8_t ", rpName, " [] = {", hexArray rps, " };\n",
      "SparseBoolMatrix ", name, " = { ", show rows, ", ",
      show columns, ", ", show (V.length rps),
      ", ", show (V.length cids), ", " , rpName, ", ", cidName, "};"]
