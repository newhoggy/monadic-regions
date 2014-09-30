module Demo.MonadicRegions where

encode :: [Int] -> String
encode nums = show nums

decode :: String -> [Int]
decode hashid = read hashid
