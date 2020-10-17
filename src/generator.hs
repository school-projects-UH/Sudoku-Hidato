import System.Random ( newStdGen, Random(randoms) )
import Printm ( printm )
import Solver ( get, getDimensions, setm, validPaths, solve )

sample :: Int -> IO [Int]
sample n = do
  gen <- newStdGen
  return $ take n $ randoms gen

buildMatrix :: Num a => Int -> Int -> [[a]]
buildMatrix rows cols = take rows $ repeat $ take cols $ repeat 0

generateRand :: Int -> IO Int
generateRand top = do
        (br:_) <- sample 1
        return (mod br top)

generate :: IO ([[Int]], (Int, Int), (Int, Int))
generate = do
    rnd1 <- generateRand 11
    let rows = 5 + rnd1 -- random rows number between [5, 15]

    rnd2 <- generateRand 11
    let cols = 5 + rnd2 -- random cols number between [5, 15]

    let m = buildMatrix rows cols
    (solutionMatrix, start, end) <- findValidStart m $ getAllPositions rows cols
    
    let freePositions = [(x, y) | x <- [0..(rows-1)], y <- [0..(cols-1)], (x, y) /= start && (x, y) /= end]
    let total = rows * cols - 2
    let k = total - div total 3
    hidato <- (removeFromMatrix solutionMatrix freePositions k)
    return (hidato, start, end)

findValidStart :: [[Int]] -> [(Int, Int)] -> IO ([[Int]], (Int, Int), (Int, Int))
findValidStart matrix freeStarts = do
    idx <- generateRand $ length freeStarts
    let start = freeStarts !! idx
    let (result, solutionMatrix, end) = findPath (setm matrix start 1) start
    let updatedFreeStarts = take idx freeStarts ++ drop (1 + idx) freeStarts
    if result then return (solutionMatrix, start, end)
    else findValidStart matrix updatedFreeStarts

getAllPositions :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
getAllPositions totalRows totalColumns = [(x, y)| x <- [0..(totalRows-1)], y <- [0..(totalColumns-1)]]

findPath :: [[Int]] -> (Int, Int) -> (Bool, [[Int]], (Int, Int))
findPath matrix currPos
    | (get matrix currPos) == (tr * tc) = (True, matrix, currPos)
    | not (null nextCells) = 
        if null validResults
            then (False, matrix, currPos)
        else head validResults
    | otherwise = (False, matrix, currPos)
    where
        (tc, tr) = getDimensions matrix
        nextCells = validPaths matrix currPos 0
        results = map (\p -> findPath (setm matrix p v)  p) nextCells
        validResults = filter (\(isValid, _, _) -> isValid) results
        v = (get matrix currPos) + 1
   
removeFromMatrix :: (Eq t, Num t, Ord a, Ord t1, Num a, Num t1, Num t2) => [[t2]] -> [(a, t1)] -> t -> IO [[t2]]
removeFromMatrix matrix _ 0 = return matrix
removeFromMatrix matrix freePositions k = do
    x <- generateRand $ length freePositions
    let updatedMatrix = setm matrix (freePositions !! x) 0
    let updatedFreePos = take x freePositions ++ drop (1 + x) freePositions
    removeFromMatrix updatedMatrix updatedFreePos (k-1)

main :: IO ()
main = do
    (hidato, start, end) <- generate

    printm  hidato
    putStr "\n"
    
    let (_, solution) = solve hidato start end 0
    printm solution