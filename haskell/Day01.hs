import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)
import Common

spelledNumbers = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

main = do
    input <- lines <$> readFile "inputs/1.txt"
    print $ sum $ map (readFirstLast . filter isDigit) input
    print $ sum $ map (readFirstLast . replaceLetters . windowTails) input

readFirstLast :: String -> Int
readFirstLast s = read [head s, last s]

windowTails :: String -> [String]
windowTails l = windowsOf 5 l ++ init (tails end)
    where w = windowsOf 5 l
          end = drop (length l - 4) l

replaceLetters :: [String] -> String
replaceLetters [] = ""
replaceLetters (w:ws)
    | isDigit (head w) = head w : replaceLetters ws
    | null start = replaceLetters ws
    | otherwise = snd (head start) : replaceLetters ws
    where start = filter (\(n, i) -> n `isPrefixOf` w) spelledNumbers
