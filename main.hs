import System.IO
import Printm

validPosition :: [[Int]] -> (Int, Int) -> Bool
validPosition board position =
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

get board position = (board !! x !! y) where (x, y) = position

replace [] _ _= []
replace (_:xs) 0 a = a:xs
replace (x:xs) n a =
  if n < 0
    then x:xs
    else x: replace xs (n-1) a

setm [] _ = []
setm (r: matrix) (x, y, v)
    | x > 0 = r: setm matrix (x-1, y, v)
    | x == 0 = (replace r y v):matrix
    | otherwise = r:matrix

main = do
    print (replace [2, 3, 4] 2 10)
