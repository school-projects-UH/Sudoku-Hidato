import Printm ( printm )
import Solver ( solve )

uhSampleBoard :: [[Int]]
uhSampleBoard =
        [ [00, 33, 35, 00, 00, -1, -1, -1]
        , [00, 00, 24, 22, 00, -1, -1, -1]
        , [00, 00, 00, 21, 00, 00, -1, -1]
        , [00, 26, 00, 13, 40, 11, -1, -1]
        , [27, 00, 00, 00, 09, 00, 01, -1]
        , [-1, -1, 00, 00, 18, 00, 00, -1]
        , [-1, -1, -1, -1, 00, 07, 00, 00]
        , [-1, -1, -1, -1, -1, -1, 05, 00]
        ]

uhSampleMinPos :: (Int, Int)
uhSampleMinPos = (4, 6)

uhSampleMaxPos :: (Int, Int)
uhSampleMaxPos = (3, 4)

main :: IO ()
main = do
    let (result, board) = solve uhSampleBoard uhSampleMinPos uhSampleMaxPos 00
    print result
    printm board