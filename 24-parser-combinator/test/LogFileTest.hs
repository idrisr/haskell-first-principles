{-# LANGUAGE QuasiQuotes #-}

module LogFileTest where

import LogFile
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ
import Text.Trifecta
import Util

logFileTests :: TestTree
logFileTests =
    testGroup
        "LogFile"
        [ dateTests
        , entryTests
        , dayTests
        , fileTests
        ]

dateTests :: TestTree
dateTests =
    testGroup
        "Date"
        [
            let sut = "# 2025-02-05"
                got = parseString parseDate mempty sut
                wot = Just (2025, 02, 05) in
                testCase "" $ maybeSuccess got @?= wot
          , let sut = "# 2025-02-07"
                got = parseString parseDate mempty sut
                wot = Just (2025, 02, 07) in
                testCase "" $ maybeSuccess got @?= wot
          , let sut = "# 2025-02-07 -- dates not nececessarily sequential"
                got = parseString parseDate mempty sut
                wot = Just (2025, 02, 07) in
                testCase "" $ maybeSuccess got @?= wot
        ]

entryTests :: TestTree
entryTests =
    testGroup
        "Entry"
        [ let sut = "08:00 Breakfast"
              got = parseString parseEntry mempty sut
              wot = Just $ Entry (8, 0) "Breakfast" ""
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "09:00 Bumped head, passed out"
              got = parseString parseEntry mempty sut
              wot = Just $ Entry (9, 0) "Bumped head" "passed out"
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "13:36 Wake up, headache"
              got = parseString parseEntry mempty sut
              wot = Just $ Entry (13, 36) "Wake up" "headache"
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "13:36 Wake up headache"
              got = parseString parseEntry mempty sut
              wot = Just $ Entry (13, 36) "Wake up headache" ""
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "08:00 Breakfast -- should I try skippin bfast?"
              got = parseString parseEntry mempty sut
              wot = Just $ Entry (8, 0) "Breakfast" ""
           in testCase "" $ maybeSuccess got @?= wot
        ]

dayTests :: TestTree
dayTests =
    testGroup
        "Day"
        [ let sut = day1
              got = parseString parseDay mempty sut
              date = (2025, 02, 05)
              entries = [
                    Entry (8, 0) "Breakfast" ""
                  , Entry (9, 0) "Sanitizing moisture collector" ""
                  ]
              wot = Just $ Day date entries
           in testCase "" $ maybeSuccess got @?= wot
        ]


fileTests :: TestTree
fileTests =
    testGroup
        "File"
        $ let
            d1 = (2025, 02, 05)
            entry1 =
                [ Entry (8, 00) "Breakfast" ""
                , Entry (9, 00) "Sanitizing moisture collector" ""
                , Entry (11, 00) "Exercising in high-grav gym" ""
                , Entry (12, 00) "Lunch" ""
                , Entry (13, 00) "Programming" ""
                , Entry (17, 00) "Commuting home in rover" ""
                , Entry (17, 30) "R&R" ""
                , Entry (19, 00) "Dinner" ""
                , Entry (21, 00) "Shower" ""
                , Entry (21, 15) "Read" ""
                , Entry (22, 00) "Sleep" ""
                ]
            day2 = (2025, 02, 07)
            entry2 =
                [ Entry (08, 00) "Breakfast" ""
                , Entry (09, 00) "Bumped head" "passed out"
                , Entry (13, 36) "Wake up" "headache"
                , Entry (13, 37) "Go to medbay" ""
                , Entry (13, 40) "Patch self up" ""
                , Entry (13, 45) "Commute home for rest" ""
                , Entry (14, 15) "Read" ""
                , Entry (21, 00) "Dinner" ""
                , Entry (21, 15) "Read" ""
                , Entry (22, 00) "Sleep" ""
                ]
            wot = Just [Day d1 entry1, Day day2 entry2]
           in
            [ let sut = file0
                  got = parseString parseFile mempty sut
               in testCase "0" $ maybeSuccess got @?= wot
               -- need to fix date with comments to consume rest of line
            , let sut = file1
                  got = parseString parseFile mempty sut
               in testCase "1" $ maybeSuccess got @?= wot
            , let sut = file2
                  got = parseString parseFile mempty sut
               in testCase "2" $ maybeSuccess got @?= wot
            , let sut = file3
                  got = parseString parseFile mempty sut
               in testCase "3" $ maybeSuccess got @?= wot
            ]

day :: String
day = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep|]

day1 :: String
day1 = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector|]

file0 :: String
file0 = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep|]

file1 :: String
file1 = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep|]

file2 :: String
file2 = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep|]

file3 :: String
file3 = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07
08:00 Breakfast
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep|]
