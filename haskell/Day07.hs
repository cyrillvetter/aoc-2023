import Data.List (sort, sortBy, sortOn, group)
import Data.Map ((!), fromList)

strengths = fromList $ zip "AKQJT98765432" [12,11..]
jokerStrengths = fromList $ zip "AKQT98765432J" [12,11..]

type Hand = (Int, Int, [Int])

main = do
    input <- map words . lines <$> readFile "inputs/7.txt"
    let handTypes = map (\[h, b] -> (read b, getHandType h, map (strengths !) h)) input
        jokerHandTypes = map (\[h, b] -> (read b, getJokerHandType h, map (jokerStrengths !) h)) input

    print $ calcTotalWinnings handTypes
    print $ calcTotalWinnings jokerHandTypes

calcTotalWinnings :: [Hand] -> Int
calcTotalWinnings s = sum $ zipWith (*) [1..] $ map (\(b, _, _) -> b) $ sortBy compareHand s

compareHand :: Hand -> Hand -> Ordering
compareHand (_, t1, s1) (_, t2, s2)
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
getJokerHandType "JJJJJ" = 6
getJokerHandType s = getHandType $ map (\c -> if c == 'J' then mostCommon else c) s
    where mostCommon = head $ last $ sortOn length $ group $ sort $ filter (/= 'J') s
