import Data.List.Split (splitOn, chunksOf)

type Mapping = (Int, Int, Int)

main = do
    input <- splitOn "\n\n" <$> readFile "inputs/5.txt"
    let seeds = parseSeeds $ head input
        maps = map parseMap $ drop 1 input
        seedRanges = map (\[a, b] -> (a, a + b)) $ chunksOf 2 seeds
    print $ minimum $ map (`mapToLocation` maps) seeds
    print $ minimum $ concatMap (\(from, to) -> mapAll from to maps) seedRanges -- ~3min execution time

mapToLocation :: Int -> [[Mapping]] -> Int
mapToLocation curr [] = curr
mapToLocation curr (m:ms) = mapToLocation (check m) ms
    where
        check :: [Mapping] -> Int
        check [] = curr
        check ((dest, source, range):xs)
            | curr >= source && curr < (source + range) = (curr - source) + dest
            | otherwise = check xs

mapAll :: Int -> Int -> [[Mapping]] -> [Int]
mapAll curr to maps
    | curr == to = []
    | otherwise = mapToLocation curr maps : mapAll (curr + 1) to maps

parseMap :: String -> [Mapping]
parseMap l = map ((\[a, b, c] -> (read a, read b, read c)) . words) $ drop 1 $ lines l

parseSeeds :: String -> [Int]
parseSeeds = map read . words . last . splitOn ": "