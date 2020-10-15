module Fillm (fillm) where

replaceEmpty [] _ _ = []
replaceEmpty list _ [] = list
replaceEmpty (x:xs) empty (y:ys)
    | x == empty = y: replaceEmpty xs empty ys
    | otherwise = x: replaceEmpty xs empty (y:ys)

fillm [] _ _ = []
fillm m [] _ = m
fillm (row:m) rep empty = do
    let prevRowReplaced = replaceEmpty row empty rep
    prevRowReplaced:fillm m (filter (\v -> not(elem v prevRowReplaced)) rep) empty