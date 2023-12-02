import Data.List.Split (splitOn)

main = do
    input <- zip [1..] . map parse . lines <$> readFile "inputs/2.txt"
    print $ sum $ map fst $ filter (isValidGame . snd) input
    print $ sum $ map (getGamePower . snd) input

isValidGame :: [(Int, String)] -> Bool
isValidGame g = hasLessThan 14 "blue" && hasLessThan 12 "red" && hasLessThan 13 "green"
    where
        hasLessThan :: Int -> String -> Bool
        hasLessThan n c = all ((<= n) . fst) $ filter ((== c) . snd) g

getGamePower :: [(Int, String)] -> Int
getGamePower g = maxOf "blue" * maxOf "red" * maxOf "green"
    where
        maxOf :: String -> Int
        maxOf c = maximum $ map fst $ filter ((== c) . snd) g

parse :: String -> [(Int, String)]
parse l = games
    where [_, g] = splitOn ": " l
          splitGames = concatMap (splitOn ", ") $ splitOn "; " g
          games = map ((\[num, col] -> (read num, col)) . words) splitGames
