import Data.Char (isDigit)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as M
import Debug.Trace (trace)

data Rule = Condition Char Char Int String | Else String deriving (Show)
type Workflow = (String, [Rule])
type Part = (Int, Int, Int, Int)

main = do
    [workflowsPart, parts] <- map lines . splitOn "\n\n" <$> readFile "inputs/19.txt"
    let workflows = M.fromList $ map parseWorkflow workflowsPart
        xmasParts = map parseXMAS parts
    print $ sum $ map (\(x, m, a, s) -> x + m + a + s) $ filter (runWorkflow workflows "in") xmasParts

runWorkflow :: M.Map String [Rule] -> String -> Part -> Bool
runWorkflow _ "A" _ = True
runWorkflow _ "R" _ = False
runWorkflow workflows curr part = runWorkflow workflows nextWorkflow part
    where rules = workflows M.! curr
          nextWorkflow = runRules rules part

runRules :: [Rule] -> Part -> String
runRules [Else dest] _ = dest
runRules ((Condition op on val next):rs) part
    | partVal `operation` val = next
    | otherwise = runRules rs part
    where partVal = getPartValue part on
          operation = if op == '<' then (<) else (>)

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
    | otherwise = Condition op (head cat) (read num) dest
    where [cat, num, dest] = splitOneOf "<>:" r
          op = if '<' `elem` r then '<' else '>'

parseXMAS :: String -> Part
parseXMAS l = (read x, read m, read a, read s)
    where [x, m, a, s] = map (filter isDigit) $ splitOn "," l
