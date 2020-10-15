import System.IO

validPosition board position = 
    row >= 0 &&
    row < length board &&
    column >= 0 &&
    column < length (board !! row)
    where (row, column) = position

formatValue :: Int -> String
formatValue value
    | l == 1 = "00" ++ (show value)
    | l == 2 = "0" ++ (show value)
    | otherwise = show value
    where l = length (show value)

formatList :: [Int] -> String
formatList [] = ""
formatList (value:list) = do
    let valuef = formatValue value
    let listf = formatList list
    if listf /= "" then valuef ++ " " ++ listf else valuef

format :: [[Int]] -> [String]
format  [] = []
format  (row:matrix) = do
    let rowf = formatList row 
    let matrixf = format matrix
    (rowf:matrixf)

matrixToString :: [[Int]] -> String
matrixToString (matrix) = unlines (format matrix)

printm matrix = putStr (matrixToString matrix)

main = do
<<<<<<< HEAD
    let z = explore [[2, 3, 4, 5], [1, 2, 3, 4]] (10, 10) (10, 10)
    print (z)
=======
    let matrix = [[1,2,3], [9,50,101]]
    printm matrix
>>>>>>> a02315e... printing matrix quite good
