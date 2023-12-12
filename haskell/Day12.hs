import Data.List.Split (splitOn)
import Data.List (group)

main = do
    input <- map parseLine . lines <$> readFile "inputs/12.txt"
    print $ sum $ map countArrangements input

countArrangements :: (String, [Int]) -> Int
countArrangements (s, i) = length $ filter (`isValidArrangements` i) $ replaceWildcards s

replaceWildcards :: String -> [String]
replaceWildcards to
    | null restPart = [until]
    | otherwise = replaceWith "#" inner ++ replaceWith "." inner
    where (until, restPart) = span (/= '?') to
          inner = replaceWildcards $ tail restPart
          replaceWith c = map ((until ++ c) ++)

isValidArrangements :: String -> [Int] -> Bool
isValidArrangements arr groups = groups == map length (filter ((== '#') . head) (group arr))

parseLine :: String -> (String, [Int])
parseLine l = (springs, groups)
    where [springs, groupPart] = words l
          groups = map read $ splitOn "," groupPart
