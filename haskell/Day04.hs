import Data.List.Split (splitOn)
import Data.List (intersect)
import qualified Data.IntMap as M

main = do
    input <- map parse . lines <$> readFile "inputs/4.txt"
    print $ sum $ map (uncurry calcCardPoints) input

    let mine = map fst input
        winning = map snd input
        cards = M.fromList $ map (\l -> (l, 1)) [1..length input]
    print $ calcTotalCards 1 mine winning cards

calcCardPoints :: [Int] -> [Int] -> Int
calcCardPoints mine winning
    | matches == 0 = 0
    | otherwise = 2 ^ (matches - 1)
    where matches = length (mine `intersect` winning)

calcTotalCards :: Int -> [[Int]] -> [[Int]] -> M.IntMap Int -> Int
calcTotalCards _ [] [] cards = sum $ M.elems cards
calcTotalCards currCard (m:ms) (w:ws) cards = calcTotalCards (currCard + 1) ms ws adjusted
    where cardsAmount = cards M.! currCard
          matches = length (m `intersect` w)
          copies = take matches [currCard+1..]
          adjusted = foldl (adjustCards cardsAmount) cards copies

adjustCards :: Int -> M.IntMap Int -> Int -> M.IntMap Int
adjustCards amount cards cardNum = M.adjust (+ amount) cardNum cards

parse :: String -> ([Int], [Int])
parse l = (mine, winning)
    where [mine, winning] = map (map read . words) $ splitOn " | " $ last $ splitOn ": " l