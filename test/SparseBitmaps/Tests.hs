module SparseBitmaps.Tests where

diag :: Int -> Int -> Int -> Int
diag v x y | x == y = v
diag _ _ _          = 0

oneElementMatrix :: Int -> Int -> Int -> Int -> Int -> Int
oneElementMatrix v x y i j | x == i, y == j = v
oneElementMatrix _ _ _ _ _                  = 0
