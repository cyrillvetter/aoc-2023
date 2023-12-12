import Data.List.Split (splitOn)
import Data.List (group)
import qualified Data.Set as S

main = do
    input <- map parseLine . lines <$> readFile "inputs/12.txt"
    print $ sum $ map (uncurry (computeArrangements "")) input

reduceDots :: String -> String
reduceDots = concatMap (\l -> if head l == '.' then "." else l) . group

isValidArrangements :: String -> [Int] -> Bool
isValidArrangements arr groups = groups == map length (filter ((== '#') . head) (group arr))

computeArrangements :: String -> String -> [Int] -> Int
computeArrangements from rest groups
    | null restPart = if isValidArrangements fromToNext groups then 1 else 0
    | otherwise = dotResult + wildResult
    where (until, restPart) = span (/= '?') rest
          fromToNext = from ++ until
          toEnd = tail restPart
          dotResult = computeArrangements (fromToNext ++ ".") toEnd groups
          wildResult = computeArrangements (fromToNext ++ "#") toEnd groups

parseLine :: String -> (String, [Int])
parseLine l = (reduceDots springs, groups)
    where [springs, groupPart] = words l
          groups = map read $ splitOn "," groupPart
