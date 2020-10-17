-- module Generator (generate) where

import System.Random
import Printm
import Solver

sample :: Int -> IO [Int]
sample n = do
  gen <- newStdGen
  return $ take n $ randoms gen

buildMatrix rows cols = take rows $ repeat $ take cols $ repeat 0

generateRand :: Int -> IO Int
generateRand top = do
        (br:_) <- sample 1
        -- return (mod br top)
        return (mod br top)

-- generate :: Int -> [[Int]]
generate = do
    -- (br1:_) <- sample 1
    -- let rows = 5 + (mod br1 26) -- random rows number between [5, 25]
    let rows = 5

    -- (br2:_) <- sample 1
    -- let cols = 5 + (mod br2 26) --  random cols number between [5, 25]
    let cols = 4

    let m = buildMatrix rows cols
    
    (solutionMatrix, start, end) <- pickOneSolution m $ pickAllValidSolutions m
    let freePositions = [(x, y) | x <- [0..(rows-1)], y <- [0..(cols-1)], (x, y) /= start && (x, y) /= end]
    let total = rows * cols - 2
    let k = total - div total 3
    hidato <- (removeFromMatrix solutionMatrix freePositions k)
    return (hidato, start, end)
    -- return solutionMatrix

-- pickAllPositions :: (Num a, Num b, Enum a, Enum b, Ord a, Eq b) => a -> b -> [((a, b), (a, b))]
pickAllPositions :: (Num a, Num b, Enum a, Enum b, Ord a, Ord b) => a -> b -> [((a, b), (a, b))]
pickAllPositions totalRows totalColumns = [((x, y), (z, w)) | x <- [0..(totalRows-1)], z <- [0..(totalRows-1)] , w <- [0..(totalColumns-1)], y <- [0..(totalColumns-1)], x < z || x == z && y < w, (x, y) /= (z, w)]

pickAllValidSolutions :: [[Int]] -> [((Int, Int), (Int, Int))]
pickAllValidSolutions matrix = filter (\(start, end) -> fst (solve (setm (setm matrix start 1) end (tr*tc)) start end 0) == True) (pickAllPositions tr tc) 
                                where (tr, tc) = getDimensions matrix


pickOneSolution matrix solutions = do
    (rnd:_) <- sample 1    
    let x = mod rnd (length solutions)
    let (start, end) = solutions !! x
    return  (snd  (solve (setm (setm matrix start 1) end (tr*tc)) start end 0), start, end) where (tr, tc) = getDimensions matrix
   
removeFromMatrix :: (Eq t, Num t, Ord a, Ord t1, Num a, Num t1, Num t2) => [[t2]] -> [(a, t1)] -> t -> IO [[t2]]
removeFromMatrix matrix _ 0 = return matrix
removeFromMatrix matrix freePositions k = do
    (rnd:_) <- sample 1
    let x = mod rnd $ length freePositions
    let updatedMatrix = setm matrix (freePositions !! x) 0
    let updatedFreePos = take x freePositions ++ drop (1 + x) freePositions
    removeFromMatrix updatedMatrix updatedFreePos (k-1)

main :: IO ()
main = do
    -- (br1:_) <- sample 1
    -- let rows = 5 + (mod br1 26) -- random rows number between [5, 25]
    
    -- (br2:_) <- sample 1
    -- let cols = 5 + (mod br2 26) --  random cols number between [5, 25]
    -- generate 0
    -- let l = pickAllPositions 25 25
    -- print $ length l
    (hidato, start, end) <- generate

    -- let m = [[0, 0], [0, 0]]
    -- print $ pickAllValidSolutions m
    printm  hidato
    let (_, solution) = solve hidato start end 0
    putStr "\n"
    printm solution