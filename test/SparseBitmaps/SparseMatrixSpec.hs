module SparseBitmaps.SparseMatrixSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V

import SparseBitmaps.SparseMatrix
import SparseBitmaps.Tests

spec :: Spec
spec = describe "Test sparse matrix functions" $ do
  it "sparse zero matrix with 1 at 1x3 diagonal matrix" $
    generateMatrix (oneElementMatrix 1 1 3) (>0) 3 3
      `shouldBe` (3, 3, V.fromList [1],
        V.fromList [V.fromList [3], V.fromList [], V.fromList []])
  it "correctly read indices from sparse matrix" $
    indices (generateMatrix (oneElementMatrix 1 1 3) (>0) 3 3)
    `shouldBe` V.fromList [(1, 3)]
