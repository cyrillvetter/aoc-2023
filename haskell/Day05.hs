import Data.List.Split (splitOn, chunksOf)

type Mapping = (Int, Int, Int)

main = do
    input <- splitOn "\n\n" <$> readFile "inputs/5.txt"
    let seeds = map read $ words $ last $ splitOn ": " $ head input
        maps = map parseMapping $ drop 1 input

    print $ minimum $ map (flip (foldl sourceToDest) maps) seeds
    print $ minimum $ concatMap (\[from, to] -> mapSeedRange from (from + to) maps) $ chunksOf 2 seeds -- ~3min execution time ):

sourceToDest :: Int -> [Mapping] -> Int
sourceToDest curr [] = curr
sourceToDest curr ((dest, source, range):xs)
    | curr >= source && curr < (source + range) = (curr - source) + dest
    | otherwise = sourceToDest curr xs

mapSeedRange :: Int -> Int -> [[Mapping]] -> [Int]
mapSeedRange curr to maps
    | curr == to = []
    | otherwise = foldl sourceToDest curr maps : mapSeedRange (curr + 1) to maps

parseMapping :: String -> [Mapping]
parseMapping = map ((\[a, b, c] -> (a, b, c)) . map read . words) . drop 1 . lines
