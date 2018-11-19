module Main where

import Codec.Picture
import System.Environment

import SparseBitmaps.ImageConv
import SparseBitmaps.SparseMatrix
import SparseBitmaps.Formats

main :: IO ()
main = do
  [file] <- getArgs
  (Right image) <- readImage file
  let grayImage = grayScale image
  let sparse = sparseMatrix grayImage 128
  let matrix = boolMatrix sparse
  putStrLn (cPlusPlusStruct (compressed matrix) "image")
