import System.IO
import Printm

validPosition board position = 
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

main = do
    let matrix = [[1,2,3], [9,50,101]]
    printm matrix
