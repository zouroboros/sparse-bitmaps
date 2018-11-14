module Encodings where

import Data.Word
import qualified Data.Vector as V
import Text.Printf

import SparseMatrix

type CompressedMatrix = (Int, Int, V.Vector Word8, V.Vector Word8)

-- | Encodes a sparse bool matrix as two byte arrays
compressed :: SparseBoolMatrix -> CompressedMatrix
compressed (rs, cols, cids, rps) = let
  crps = V.map fromIntegral rps
  is = [0 .. (V.length rps)]
  marker x = V.fromList [256]
  row i | before i <= first i = V.slice ((V.!) rps i) ((V.!) rps (i + 1) - (V.!) rps i) cids
  row i                           = (V.++) (marker i) (V.slice ((V.!) rps i) ((V.!) rps (i + 1) - (V.!) rps i) cids)
  before i = (V.!) cids (((V.!) rps i) - 1)
  first i = (V.!) cids ((V.!) rps i)
  ccids = V.map (fromIntegral . (\x -> x - 1)) (V.concat (map row is))
  in (rs, cols, ccids, crps)

hexArray :: V.Vector Word8 -> String
hexArray v = concatMap (printf "%x") (V.toList v)

cPlusPlusArray :: CompressedMatrix -> String -> String
cPlusPlusArray (rows, columns, cids, rps) name = concat
  ["const uint8_t ", name, "_column_indices [] = { ", hexArray cids, " };\n",
   "const uint8_t ", name, "_row_pointer [] = {", hexArray rps, " };\n"]
