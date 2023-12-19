import Data.Char (isDigit)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as M

data Rule = Conditional Char Char Int String | Else String deriving (Show)
type Workflow = (String, [Rule])
type Part = (Int, Int, Int, Int)
type Range = (Int, Int)
type RangePart = (Range, Range, Range, Range)

main = do
    [workflowsPart, parts] <- map lines . splitOn "\n\n" <$> readFile "inputs/19.txt"
    let workflows = M.fromList $ map parseWorkflow workflowsPart
        xmasParts = map parsePart parts
    print $ sum $ map (\(x, m, a, s) -> x + m + a + s) $ filter (isPartAccepted workflows "in") xmasParts
    print $ traverseWorkflow workflows "in" ((1, 4000), (1, 4000), (1, 4000), (1, 4000))

traverseWorkflow :: M.Map String [Rule] -> String -> RangePart -> Int
traverseWorkflow _ "A" range = calcRangeResult range
traverseWorkflow _ "R" range = 0
traverseWorkflow rules curr range = traverseRules (rules M.! curr) range
    where
        traverseRules :: [Rule] -> RangePart -> Int
        traverseRules [Else next] range = traverseWorkflow rules next range
        traverseRules ((Conditional op on val next):rs) range = leftResult + rightResult
            where (leftRange, rightRange) = adaptRangePart range on op val
                  leftResult = traverseWorkflow rules next leftRange
                  rightResult = traverseRules rs rightRange

adaptRangePart :: RangePart -> Char -> Char -> Int -> (RangePart, RangePart)
adaptRangePart (x, m, a, s) 'x' op v = ((lowerRange op x v, m, a, s), (upperRange op x v, m, a, s))
adaptRangePart (x, m, a, s) 'm' op v = ((x, lowerRange op m v, a, s), (x, upperRange op m v, a, s))
adaptRangePart (x, m, a, s) 'a' op v = ((x, m, lowerRange op a v, s), (x, m, upperRange op a v, s))
adaptRangePart (x, m, a, s) 's' op v = ((x, m, a, lowerRange op s v), (x, m, a, upperRange op s v))

lowerRange :: Char -> Range -> Int -> Range
lowerRange '<' (l, r) val = (l, val - 1)
lowerRange '>' (l, r) val = (val + 1, r)

upperRange :: Char -> Range -> Int -> Range
upperRange '<' (l, r) val = (val, r)
upperRange '>' (l, r) val = (l, val)

calcRangeResult :: RangePart -> Int
calcRangeResult ((lx, rx), (lm, rm), (la, ra), (ls, rs)) = (rx - lx + 1) * (rm - lm + 1) * (ra - la + 1) * (rs - ls + 1)

isPartAccepted :: M.Map String [Rule] -> String -> Part -> Bool
isPartAccepted _ "A" _ = True
isPartAccepted _ "R" _ = False
isPartAccepted workflows curr part = isPartAccepted workflows nextWorkflow part
    where rules = workflows M.! curr
          nextWorkflow = runRules rules part

runRules :: [Rule] -> Part -> String
runRules [Else dest] _ = dest
runRules ((Conditional op on val next):cs) part
    | partVal `ruleOp` val = next
    | otherwise = runRules cs part
    where partVal = getPartValue part on
          ruleOp = if op == '<' then (<) else (>)

getPartValue :: Part -> Char -> Int
getPartValue (x, _, _, _) 'x' = x
getPartValue (_, m, _, _) 'm' = m
getPartValue (_, _, a, _) 'a' = a
getPartValue (_, _, _, s) 's' = s

parseWorkflow :: String -> Workflow
parseWorkflow s = (name, rules)
    where (name:rulesPart:_) = splitOneOf "{}" s
          rules = map parseRule $ splitOn "," rulesPart

parseRule :: String -> Rule
parseRule r
    | ':' `notElem` r = Else r
    | otherwise = Conditional op (head cat) (read num) dest
    where [cat, num, dest] = splitOneOf "<>:" r
          op = if '<' `elem` r then '<' else '>'

parsePart :: String -> Part
parsePart l = (x, m, a, s)
    where [x, m, a, s] = map (read . filter isDigit) (splitOn "," l)
