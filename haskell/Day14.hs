import Data.List (transpose)

main = do
    input <- transpose . lines <$> readFile "inputs/14.txt"
    print $ sum $ map (calcWeight (length input)) input

calcWeight :: Int -> String -> Int
calcWeight solid row
    | null toEnd = currSum
    | otherwise = currSum + calcWeight nextSolid (tail toEnd)
    where (until, toEnd) = span (/= '#') row
          nextSolid = solid - length until - 1
          rocksAmount = length $ filter (== 'O') until
          currSum = sum [solid,solid-1..solid-rocksAmount+1]
