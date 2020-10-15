import System.IO
import Printm

validPosition :: [[Int]] -> (Int, Int) -> Bool
validPosition board position =
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

explore :: [[Int]] -> (Int,Int) -> (Int,Int) -> (Bool, [[Int]])
explore board position targetPosition
    | position == targetPosition = (True, board)
    | not(validPosition board position) = (False, board)


main = do
    let z = explore [[1, 2, 3]] (-1, 10) (10, 10)
    print z
