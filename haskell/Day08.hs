import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main = do
    [instr, other] <- splitOn "\n\n" <$> readFile "inputs/8.txt"
    let network = M.fromList $ map parse $ lines other
        cycledInstrs = cycle instr
        aNodes = filter ((== 'A') . last) (M.keys network)

    print $ firstZOccurrence "AAA" cycledInstrs network 0
    print $ foldl1 lcm $ map (\v -> firstZOccurrence v cycledInstrs network 0) aNodes

firstZOccurrence :: String -> String -> M.Map String (String, String) -> Int -> Int
firstZOccurrence [_, _, 'Z'] _ _ steps = steps
firstZOccurrence element (c:cs) network steps = firstZOccurrence nextElem cs network (steps + 1)
    where nextElem = pick $ network M.! element
          pick = if c == 'R' then snd else fst

parse :: String -> (String, (String, String))
parse s = (from, (tail left, init right))
    where [from, to] = splitOn " = " s
          [left, right] = splitOn ", " to
