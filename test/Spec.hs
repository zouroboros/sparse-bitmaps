module Main where

import Test.Hspec
import qualified Data.Vector as V

import Encodings
import SparseMatrix

diag :: Int -> Int -> Int -> Int
diag v x y | x == y = v
diag _ _ _          = 0

oneElementMatrix :: Int -> Int -> Int -> Int -> Int -> Int
oneElementMatrix v x y i j | x == i, y == j = v
oneElementMatrix _ _ _ _ _                  = 0

main :: IO ()
main = hspec $
  describe "Tests" $ do
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
    it "sparse zero matrix with 1 at 1x3 diagonal matrix" $
      generateMatrix (oneElementMatrix 1 1 3) (>0) 3 3
        `shouldBe` (3, 3, V.fromList [1], V.fromList [V.fromList [3], V.fromList [], V.fromList []])
    it "indices" $
      indices (generateMatrix (oneElementMatrix 1 1 3) (>0) 3 3)
      `shouldBe` V.fromList [(1, 3)]
