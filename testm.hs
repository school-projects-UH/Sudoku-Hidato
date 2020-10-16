module Testm (test) where

validPosition :: [[Int]] -> (Int, Int) -> Bool
validPosition board position =
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

get board position 
    | validPosition board position = (board !! x !! y)
    | otherwise = -1 
    where (x, y) = position

replace [] _ _= []
replace (_:xs) 0 a = a:xs
replace (x:xs) n a =
  if n < 0
    then x:xs
    else x: replace xs (n-1) a

setm [] _ _ _ = []
setm (r: matrix) x y v
    | x > 0 = r: setm matrix (x-1) y v
    | x == 0 = (replace r y v):matrix
    | otherwise = r:matrix

adj a b r c = [(x+a, y+b) | x <- [-1, 0, 1], y <- [-1, 0, 1], x/=0 || y/=0 , x+a < r, x+a >=0, y+b < c, y+b >= 0]

verifySucesor matrix currentPos = do
    let sucesor = filter (\pos -> (get matrix pos) == ((get matrix currentPos) + 1)) (adj a b r c)
    if sucesor == [] then (-1, -1)
    else sucesor !! 0
    where ((a, b), r, c) = (currentPos, length matrix, length (matrix!!0))

test matrix currentPos targetPos
    | currentPos == targetPos = True
    | sucesorPos /= (-1, -1) = test matrix sucesorPos targetPos
    | otherwise = False
    where sucesorPos = verifySucesor matrix currentPos