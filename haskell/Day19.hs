import Data.Char (isDigit)
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Map as M

data Rule = Condition Char Char Int String | Else String deriving (Show)
type Workflow = (String, [Rule])
type Part = (Int, Int, Int, Int)

main = do
    [workflowsPart, parts] <- map lines . splitOn "\n\n" <$> readFile "inputs/19.txt"
    let workflows = M.fromList $ map parseWorkflow workflowsPart
        xmasParts = map parsePart parts
    print $ sum $ map (\(x, m, a, s) -> x + m + a + s) $ filter (isPartAccepted workflows "in") xmasParts

isPartAccepted :: M.Map String [Rule] -> String -> Part -> Bool
isPartAccepted _ "A" _ = True
isPartAccepted _ "R" _ = False
isPartAccepted workflows curr part = isPartAccepted workflows nextWorkflow part
    where rules = workflows M.! curr
          nextWorkflow = runRules rules part

runRules :: [Rule] -> Part -> String
runRules [Else dest] _ = dest
runRules ((Condition op on val next):cs) part
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
    | otherwise = Condition op (head cat) (read num) dest
    where [cat, num, dest] = splitOneOf "<>:" r
          op = if '<' `elem` r then '<' else '>'

parsePart :: String -> Part
parsePart l = (x, m, a, s)
    where [x, m, a, s] = map (read . filter isDigit) (splitOn "," l)
