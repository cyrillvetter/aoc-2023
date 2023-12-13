import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromJust, fromMaybe)

main = do
    input <- map lines . splitOn "\n\n" <$> readFile "inputs/13.txt"
    print $ sum $ map (getReflectionValue 0) input
    print $ sum $ map (getReflectionValue 1) input

getReflectionValue :: Int -> [String] -> Int
getReflectionValue smudges mirror = fromMaybe rowReflection colReflection
    where colReflection = getReflectionIndex smudges $ transpose mirror
          rowReflection = 100 * fromJust (getReflectionIndex smudges mirror)

getReflectionIndex :: Int -> [String] -> Maybe Int
getReflectionIndex smudges mirror = (\(i, _, _) -> i) <$> find isReflective zipped
    where zipped = zip3 [1..] mirror $ tail mirror
          isReflective (i, to, from) = countDifferences to from <= smudges && countSmudges i mirror == smudges

countSmudges :: Int -> [String] -> Int
countSmudges i l = sum $ zipWith countDifferences (reverse from) to
    where (from, to) = splitAt i l

countDifferences :: String -> String -> Int
countDifferences x y = length $ filter (uncurry (/=)) $ zip x y
