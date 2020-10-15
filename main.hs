import System.IO

validPosition board position = 
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

main = do
    let z = explore [[2, 3, 4, 5], [1, 2, 3, 4]] (10, 10) (10, 10)
    print (z)
