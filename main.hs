import System.IO
import Printm
import Fillm
import Data.Foldable (toList)
import Data.Sequence (fromList)

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

setm :: (Num a, Num t1, Ord a, Ord t1) => [[t2]] -> (a, t1, t2) -> [[t2]]
setm [] _ = []
setm (r: matrix) (x, y, v)
    | x > 0 = r: setm matrix (x-1, y, v)
    | x == 0 = (replace r y v):matrix
    | otherwise = r:matrix

main = do
    let m1 = [[60, 0, 0], [0, 50, 60], [0, 0, 0], [0, 0, 70]]
    let fm1 = fillm m1 [1,2,3,4,5,6,7,8] 0
    printm fm1