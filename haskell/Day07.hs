import Data.List (sort, sortBy, sortOn, group)
import qualified Data.Map as M

strengths = M.fromList $ zip "AKQJT98765432" [12,11..]
jokerStrengths = M.fromList $ zip "AKQT98765432J" [12,11..]

type Hand = (String, Int, Int, [Int])

main = do
    input <- map words . lines <$> readFile "inputs/7.txt"
    let handTypes = map (\[h, b] -> (h, read b, getHandType h, map (strengths M.!) h)) input
        jokerHandTypes = map (\[h, b] -> (h, read b, getJokerHandType h, map (jokerStrengths M.!) h)) input

    print $ calcTotalWinnings handTypes
    print $ calcTotalWinnings jokerHandTypes

calcTotalWinnings :: [Hand] -> Int
calcTotalWinnings s = sum $ zipWith (*) [1..] $ map (\(_, b, _, _) -> b) $ sortBy compareHand s

compareHand :: Hand -> Hand -> Ordering
compareHand (_, _, t1, s1) (_, _, t2, s2)
    | primary == EQ = compare s1 s2
    | otherwise = primary
    where primary = compare t1 t2

getHandType :: String -> Int
getHandType s = case sort $ map length $ group $ sort s of
    [5]          -> 6
    [1, 4]       -> 5
    [2, 3]       -> 4
    [1, 1, 3]    -> 3
    [1, 2, 2]    -> 2
    [1, 1, 1, 2] -> 1
    _            -> 0

getJokerHandType :: String -> Int
getJokerHandType s
    | s == "JJJJJ" = 6
    | otherwise = getHandType $ map (\c -> if c == 'J' then mostCommon else c) s
    where mostCommon = head $ last $ sortOn length $ group $ sort $ filter (/= 'J') s
