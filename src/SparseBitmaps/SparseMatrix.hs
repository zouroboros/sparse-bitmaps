module SparseBitmaps.SparseMatrix where

import Control.Monad (join)
import qualified Data.Vector as V
import Data.List (foldl')

import Debug.Trace

-- | Defines a container for a sparse column compressed matrix
type SparseMatrix a =
  -- rows, columns, values, row vector
  (Int, Int, V.Vector a, V.Vector (V.Vector Int))

type SparseBoolMatrix = (Int, Int, V.Vector (V.Vector Int))

boolMatrix :: SparseMatrix a -> SparseBoolMatrix
boolMatrix (rs, cols, _, rc) = (rs, cols, rc)

emptyMatrix :: Int -> Int -> SparseMatrix a
emptyMatrix rows columns = (rows, columns, V.empty, V.empty)

rows :: SparseMatrix a -> Int
rows (rows, _, _, _) = rows

columns :: SparseMatrix a -> Int
columns (_, columns, _ , _) = columns

values :: SparseMatrix a -> V.Vector a
values (_, _, values, _) = values

-- | Returns a list of indices that have a value
indices :: SparseMatrix a -> V.Vector (Int, Int)
indices (_, _, _, rowColumn) = indices' rowColumn

indices' :: V.Vector (V.Vector Int) -> V.Vector (Int, Int)
indices' rowsColumns = join
  (V.imap (\i column -> V.map (\j -> (i + 1, j)) column) rowsColumns)

-- | Creates a sparse matrix for given generator function, filter,
--   number of rows and number of columns. A values that pass the filter are
--   included in the matrix.
generateMatrix :: (Int -> Int -> a) -> (a -> Bool) -> Int -> Int
  -> SparseMatrix a
generateMatrix gen fil rs cols = let
  column i = V.fromList (map (fst) (filter (\(j, x) -> fil x) [(j, gen i j) | j <- [1 .. cols]]))
  rowColumns = V.fromList (map column [1 .. rs])
  values = V.map (uncurry gen) (indices' rowColumns)
  in (rs, cols, values, rowColumns)
