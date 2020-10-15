import System.IO
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

succPos adjCells succValue = filter (\(v, _, _) -> v == succValue) adjCells

emptyPos adjCells = filter (\(v, _, _) -> v == 0) adjCells

move :: [[Int]] -> (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> (Int, Int, Int) -> (Bool, [[Int]])
move board targetPos [] emptyPositions nextPos
    | elem nextPos emptyPositions = solve (setm board (x,y,v)) (x,y) targetPos
    | otherwise = (False, board)
    where (v, x, y) = nextPos

move board targetPos (succ:[]) emptyPositions nextPos
    | tx == x && ty == y = (True, board)
    | x == sx && y == sy = solve (setm board (x,y,v)) (x,y) targetPos
    | otherwise = (False, board)
    where (_, sx, sy) = succ
          (v, x, y) = nextPos
          (tx, ty) = targetPos

solve board pos targetPos = do
    let curr = get board pos
    let n = (get board (x-1, y), x-1, y)  
    let ne = (get board (x-1, y+1), x-1, y+1)  
    let e = (get board (x, y+1), x, y+1)  
    let se = (get board (x+1, y+1), x+1, y+1)  
    let s = (get board (x+1, y), x+1, y)  
    let sw = (get board (x+1, y-1), x+1, y-1)  
    let w = (get board (x, y-1), x, y-1)  
    let nw = (get board (x-1, y-1), x-1, y-1)  

    let adjCells = [n,ne,e,se,sw,w,nw]
    let succPosition = succPos adjCells (curr+1)
    let emptyPositions = emptyPos adjCells

    let (v1, b1) = move board targetPos succPosition emptyPositions n
    let (v2, b2) = move board targetPos succPosition emptyPositions ne
    let (v3, b3) = move board targetPos succPosition emptyPositions e
    let (v4, b4) = move board targetPos succPosition emptyPositions se
    let (v5, b5) = move board targetPos succPosition emptyPositions sw
    let (v6, b6) = move board targetPos succPosition emptyPositions w
    let (v7, b7) = move board targetPos succPosition emptyPositions nw
    
    if v1 then (v1,b1)
    else if v2 then (v2,b2)
    else if v3 then (v3,b3)
    else if v4 then (v4,b4)
    else if v5 then (v5,b5)
    else if v6 then (v6,b6)
    else if v7 then (v7,b7)
    else (False, board)

    where (x, y) = pos

main = do
    -- let m1 = [[12, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 1]]
    -- let (result, board) = solve m1 (3,2) (0,0)
    let m1 = [[1, 0], [3, 4]]
    let (result, board) = solve m1 (0,0) (1,1)
    print result
    printm board