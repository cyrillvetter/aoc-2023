import Data.List (sort, sortBy, sortOn, group)
import qualified Data.Map as M
import Common (toTuple)
import Debug.Trace (trace)

strengths = M.fromList [('A', 12), ('K', 11), ('Q', 10), ('J', 9), ('T', 8), ('9', 7), ('8', 6), ('7', 5), ('6', 4), ('5', 3), ('4', 2), ('3', 1), ('2', 0)]

main = do
    input <- map (toTuple . words) . lines <$> readFile "inputs/7.txt"
    let sorted = map (read . snd) $ sortBy (\(a, _) (b, _) -> cmp a b) input
    print $ sum $ zipWith (*) [1..] sorted

cmp :: String -> String -> Ordering
cmp x y
    | primary == EQ = secondaryCompare x y
    | otherwise = primary
    where xt = getType x
          yt = getType y
          primary = compare xt yt

getType :: String -> Int
getType c
    | amt == 1 = 6
    | amt == 2 && length first == 1 = 5
    | amt == 2 && length first == 2 = 4
    | amt == 3 && length first == 1 && length second == 1 = 3
    | amt == 3 && length first == 1 && length second == 2 = 2
    | amt == 4 && length fourth == 2 = 1
    | otherwise = 0
    where grp = sortOn length $ group $ sort c
          amt = length grp
          first = head grp
          second = grp !! 1
          third = grp !! 2
          fourth = grp !! 3

secondaryCompare :: String -> String -> Ordering
secondaryCompare (x:xs) (y:ys)
    | cmp == EQ = secondaryCompare xs ys
    | otherwise = cmp
    where xStren = strengths M.! x
          yStren = strengths M.! y
          cmp = xStren `compare` yStren
