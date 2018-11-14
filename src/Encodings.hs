module Encodings where

import Data.Word
import qualified Data.Vector as V
import Text.Printf
import Data.List

import SparseMatrix

import Debug.Trace

type CompressedMatrix = (Int, Int, V.Vector Word8, V.Vector Word8)

-- | Encodes a sparse bool matrix as two byte arrays
compressed :: SparseBoolMatrix -> CompressedMatrix
compressed (rs, cols, cids, rps) = let
  crps = V.map fromIntegral rps
  is = [0 .. V.length rps - 1]
  marker x = V.fromList [256]
  rlen i | i < rs - 1 = (V.!) rps (i + 1) - (V.!) rps i
  rlen i              = V.length cids - (V.!) rps i + 1
  row i | before i <= first i = V.slice ((V.!) rps i - 1) (rlen i) cids
  row i                       = (V.++) (marker i) (V.slice ((V.!) rps i - 1) (rlen i) cids)
  before 0 = 0
  before i = (V.!) cids ((V.!) rps i - 2)
  first i = (V.!) cids ((V.!) rps i - 1)
  ccids = V.map (fromIntegral . (\x -> x - 1)) (V.concat (map row is))
  in (rs, cols, ccids, V.map (\x -> x - 1) crps)

hexArray :: V.Vector Word8 -> String
hexArray v = intercalate "," (map (printf "%#x") (V.toList v))

cPlusPlusArray :: CompressedMatrix -> String -> String
cPlusPlusArray (rows, columns, cids, rps) name = concat
  ["const uint8_t ", name, "_column_indices [] = { ", hexArray cids, " };\n",
   "const uint8_t ", name, "_row_pointer [] = {", hexArray rps, " };\n"]
