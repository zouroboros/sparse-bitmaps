module SparseMatrix where

import qualified Data.Vector as V
import Data.List (foldl')

type SparseMatrix a =
  -- rows, columns, values, column indices, row pointer
  (Int, Int, V.Vector a, V.Vector Int, V.Vector Int)

emptyMatrix :: Int -> Int -> SparseMatrix a
emptyMatrix rows columns = (rows, columns, V.empty, V.empty, V.empty)

rows :: SparseMatrix a -> Int
rows (rows, _, _, _, _) = rows

columns :: SparseMatrix a -> Int
columns (_, columns, _ , _, _) = columns

values :: SparseMatrix a -> V.Vector a
values (_, _, values, _ , _) = values

ci :: SparseMatrix a -> V.Vector Int
ci (_, _, _, cis, _) = cis

rp :: SparseMatrix a -> V.Vector Int
rp (_, _, _, _, rps) = rps

-- | Creates a sparse matrix for given generator function, filter,
--   number of rows and number of columns. A values that pass the filter are
--   included in the matrix.
generateMatrix :: (Int -> Int -> a) -> (a -> Bool) -> Int -> Int
  -> SparseMatrix a
generateMatrix gen fil rs cols = let
  ijs = [(i, j) | i <- [1 .. rs], j <- [1 .. cols]]
  vals =  V.fromList (filter fil (map (uncurry gen) ijs))
  cis = V.replicate (V.length vals) 0
  rps = V.replicate rs 0

  updateRP vec r i | (V.!) vec r == 0 = (V.//) vec [(r, i)]
  updateRP vec _ _                    = vec

  updateM (i', cil, m) (i, j)  | not (fil (gen i j)) = (i, cil, m)
  updateM (i', cil, m) (i, j)  | i == i'
    = (i, cil + 1, (rs, cols, vals, (V.//) (ci m) [(cil - 1, j)],
     updateRP (rp m) (i - 1) cil))
  updateM (i', cil, m) (i, j)
    = (i, cil + 1, (rs, cols, vals, (V.//) (ci m) [(cil - 1, j)],
     updateRP (rp m) (i - 1) cil))
  (_, _, matrix) = foldl' updateM (0, 1, (rs, cols, vals, cis, rps)) ijs
  in matrix
