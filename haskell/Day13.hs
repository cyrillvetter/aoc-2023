import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromJust, fromMaybe)

main = do
    input <- map lines . splitOn "\n\n" <$> readFile "inputs/13.txt"
    print $ sum $ map (getReflectionValue 0) input
    print $ sum $ map (getReflectionValue 1) input

getReflectionValue :: Int -> [String] -> Int
getReflectionValue smudges mirror = fromMaybe rowReflection colReflection
    where cols = transpose mirror
          colReflection = getReflectionIndex smudges cols
          rowReflection = 100 * fromJust (getReflectionIndex smudges mirror)

getReflectionIndex :: Int -> [String] -> Maybe Int
getReflectionIndex smudges mirror = (\(i, _, _) -> i) <$> find isReflective zipped
    where zipped = zip3 [1..] mirror $ tail mirror
          isReflective (i, to, from) = countDifferences to from <= smudges && countReflectiveDifferences i mirror == smudges

countReflectiveDifferences :: Int -> [String] -> Int
countReflectiveDifferences i l = sum $ zipWith countDifferences to from
    where to = drop i l
          from = reverse $ take i l

countDifferences :: String -> String -> Int
countDifferences x y = length $ filter (uncurry (/=)) $ zip x y
