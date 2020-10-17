module Solver (solve, setm, getDimensions, get, validPaths) where
import Printm

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

getDimensions matrix = (length matrix, maximum (map(\list -> length list) matrix))

replace [] _ _= []
replace (_:xs) 0 a = a:xs
replace (x:xs) n a =
  if n < 0
    then x:xs
    else x: replace xs (n-1) a

setm [] _ _  = []
setm (r: matrix) pos v
    | x > 0 = r: setm matrix (x-1, y) v
    | x == 0 = (replace r y v):matrix
    | otherwise = r:matrix
    where (x, y) = pos

adj matrix pos = [(x+a, y+b) | x <- [-1, 0, 1], y <- [-1, 0, 1], x/=0 || y/=0 , x+a < r, x+a >=0, y+b < c, y+b >= 0] where ((a, b), (r, c)) = (pos, getDimensions matrix)
validPaths matrix pos empty
    | not(null l1) = l1
    | otherwise = filter (\p -> ((get matrix p) == empty)) (adj matrix pos)
    where l1 = filter (\p -> ((get matrix p) == ((get matrix pos) + 1))) (adj matrix pos)

solve :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int -> (Bool, [[Int]])
solve matrix start end empty
    | start == end = (True, matrix)
    | not (null nextCells) = 
        if null validResults
            then (False, matrix)
        else head validResults
    | otherwise = (False, matrix)

    where 
        nextCells = validPaths matrix start empty
        results = map (\p -> solve (setm matrix p v)  p end empty) nextCells
        validResults = filter (\(r, _) -> r == True) results
        v = (get matrix start) + 1