module Printm (printm) where

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