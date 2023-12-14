import Data.List (transpose, intercalate)
import Debug.Trace (trace)

main = do
    input <- transpose . lines <$> readFile "inputs/14.txt"
    putStrLn $ intercalate "\n" input
    print $ map (\w -> calcRowWeight w (length input)) input

calcRowWeight :: String -> Int -> Int
calcRowWeight row width = calcWeight (zip [width,width-1..] row) width
    where
        calcWeight :: [(Int, Char)] -> Int -> Int
        calcWeight n curr
            | null toEnd = sum $ take rocksAmount [curr,curr-1..]
            | otherwise = sum (take rocksAmount [curr,curr-1..]) + calcWeight (tail toEnd) (fst (head toEnd))
            where (until, toEnd) = span ((/= '#') . snd) n
                  rocksAmount = length $ filter ((== 'O') . snd) until
