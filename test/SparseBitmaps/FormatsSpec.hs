module SparseBitmaps.FormatsSpec where

import Test.Hspec
import qualified Data.Vector as V

import SparseBitmaps.Formats
import SparseBitmaps.SparseMatrix
import SparseBitmaps.Tests

spec :: Spec
spec = describe "Test formats and encodings" $ do
    it "compress 3x3 diagonal matrix" $
      compressed (boolMatrix $ generateMatrix (diag 1) (>0) 3 3)
        `shouldBe` (3, 3, V.fromList [0, 1, 2], V.fromList [0, 1, 2])
    it "compress zero matrix with 1 at 1x3 diagonal matrix" $
      compressed (boolMatrix $ generateMatrix (oneElementMatrix 1 1 3) (>0) 3 3)
        `shouldBe` (3, 3, V.fromList [2], V.fromList [0, 1, 1])
    it "compress 256x256 with 1 at 256x256" $
      compressed (boolMatrix $ generateMatrix (oneElementMatrix 1 256 256) (>0) 256 256)
        `shouldBe` (256, 256, V.fromList [255, 0], V.replicate 256 0)
    it "compressV [256]" $
      compressV (V.fromList [0, 256, 257, 258]) `shouldBe` V.fromList [0, 255, 1, 2, 3]
