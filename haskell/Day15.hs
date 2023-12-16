import Data.List.Split (splitOn, splitOneOf)
import Data.List (lookup)
import Data.Char (ord)
import qualified Data.IntMap as M

type Lens = (String, Int)

main = do
    input <- splitOn "," <$> readFile "inputs/15.txt"
    print $ sum $ map hash input
    print $ configureLenses input $ M.fromAscList $ map (\v -> (v, [])) [0..255]

hash :: String -> Int
hash = foldl (\curr c -> (17 * (curr + ord c)) `mod` 256) 0

configureLenses :: [String] -> M.IntMap [Lens] -> Int
configureLenses [] lenses = sum $ map calcVal $ M.assocs lenses
    where
        calcVal :: (Int, [Lens]) -> Int
        calcVal (boxNum, lenses) = sum $ zipWith (\s (_, f) -> (boxNum + 1) * s * f) [1..] lenses
configureLenses (x:xs) lenses
    | isSign = configureLenses xs $ removeLens box label lenses
    | otherwise = configureLenses xs $ replaceLens box label (read focalLength) lenses
    where isSign = '-' `elem` x
          [label, focalLength] = splitOneOf "-=" x
          box = hash label

removeLens :: Int -> String -> M.IntMap [Lens] -> M.IntMap [Lens]
removeLens box label = M.adjust (filter ((/= label) . fst)) box

replaceLens :: Int -> String -> Int -> M.IntMap [Lens] -> M.IntMap [Lens]
replaceLens box label focalLen = M.adjust repl box
    where
        repl :: [Lens] -> [Lens]
        repl lenses = case label `lookup` lenses of
            Just _  -> map (\(l, f) -> if l == label then (l, focalLen) else (l, f)) lenses
            Nothing -> lenses ++ [(label, focalLen)]

