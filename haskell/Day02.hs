import Data.List.Split (splitOn)

data Color = Blue | Red | Green deriving (Enum, Eq)

main = do
    input <- map parse . lines <$> readFile "inputs/2.txt"
    print $ sum $ map fst $ filter (isValidGame . concat . snd) input
    print $ sum $ map (getGamePower . concat . snd) input
    print "Day 2"

isValidGame :: [(Int, String)] -> Bool
isValidGame g = has 14 "blue" && has 12 "red" && has 13 "green"
    where
          has :: Int -> String -> Bool
          has n c = all ((<= n) . fst) $ filter ((== c) . snd) g

getGamePower :: [(Int, String)] -> Int
getGamePower g = maxOf "blue" * maxOf "red" * maxOf "green"
    where
        maxOf :: String -> Int
        maxOf c = maximum $ map fst $ filter ((== c) . snd) g

parse :: String -> (Int, [[(Int, String)]])
parse l = (gameNum, games)
    where [numPart, rest] = splitOn ": " l
          gameNum = read $ last $ words numPart
          games = map (parseGame . splitOn ", ") $ splitOn "; " rest

parseGame :: [String] -> [(Int, String)]
parseGame = map p
    where p = (\[num, col] -> (read num, col)) . words
