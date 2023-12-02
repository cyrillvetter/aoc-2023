import Data.Char (isDigit)
import Data.List (isPrefixOf, tails, find)

nums = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

main = do
    input <- lines <$> readFile "inputs/1.txt"
    print $ sum $ map (readFirstLast . filter isDigit) input
    print $ sum $ map (readFirstLast . replaceLetters . tails) input

readFirstLast :: String -> Int
readFirstLast s = read [head s, last s]

replaceLetters :: [String] -> String
replaceLetters [""] = ""
replaceLetters (w:ws)
    | isDigit (head w) = head w : replaceLetters ws
    | otherwise = case find ((`isPrefixOf` w) . fst) nums of
        Just (_, c) -> c : replaceLetters ws
        Nothing -> replaceLetters ws
