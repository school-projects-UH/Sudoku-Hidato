import System.IO
import Printm
import Fillm
import Tests
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Solver


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

uhSampleMinPos = (4, 6)
uhSampleMaxPos = (3, 4)

main = do
    -- runSamples [(uhSampleBoard, uhSampleMinPos, uhSampleMaxPos, 0)]
    -- let (m, s, e, empty) = head tests
    -- let (result, board) = solve m s e empty
    -- print result
    -- printm board

    let (result, board) = solve uhSampleBoard uhSampleMinPos uhSampleMaxPos 00
    print result
    printm board