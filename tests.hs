module Tests (tests) where

uhSampleBoard =
        [ [00, 33, 35, 00, 00, -1, -1, -1]
        , [00, 00, 24, 22, 00, -1, -1, -1]
        , [00, 00, 00, 21, 00, 00, -1, -1]
        , [00, 26, 00, 13, 40, 11, 11, -1]
        , [27, 00, 00, 00, 09, 00, 01, -1]
        , [-1, -1, 00, 00, 18, 00, 00, -1]
        , [-1, -1, -1, -1, 00, 07, 00, 00]
        , [-1, -1, -1, -1, -1, -1, 05, 00]
        ]

uhSampleMinPos = (4, 6)
uhSampleMaxPos = (3, 4)

uhSample = (uhSampleBoard, uhSampleMinPos, uhSampleMaxPos, 00)

tests = [uhSample]