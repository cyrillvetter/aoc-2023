import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import qualified Data.Map as M
import Debug.Trace (trace)

type Element = (String, Pulse)
data Pulse = High | Low deriving (Enum, Eq, Show)
data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction [Element] [String] deriving (Show)

startName = "broadcaster"

main = do
    input <- map parseModule . lines <$> readFile "inputs/20.txt"
    let modules = setConjuntionInputs input $ map (second getOutputs) input
        moduleMap = M.fromList modules
        broadcast = map (\s -> (startName, (s, Low))) $ getOutputs $ snd $ head $ filter ((== startName) . fst) modules
    print $ repeatProcess broadcast moduleMap

repeatProcess :: [(String, Element)] -> M.Map String Module -> Int
repeatProcess start modules = low * high
    where (_, (low, high)) = iterate (\(m, (low, high)) -> process start m (low + 1, high)) (modules, (0, 0)) !! 1000

process :: [(String, Element)] -> M.Map String Module -> (Int, Int) -> (M.Map String Module, (Int, Int))
process [] modules count = (modules, count)
process ((prev, (name, pulse)):es) modules count@(low, high) = case element of
    Just (FlipFlop on out)        -> handleFlipFlop pulse name on out es modules count
    Just (Conjunction memory out) -> handleConjunction pulse prev name memory out es modules count
    _                             -> process es modules (if pulse == Low then (low + 1, high) else (low, high + 1))
    where element = name `M.lookup` modules

handleFlipFlop :: Pulse -> String -> Bool -> [String] -> [(String, Element)] -> M.Map String Module -> (Int, Int) -> (M.Map String Module, (Int, Int))
handleFlipFlop High _ _ _ remaining modules (low, high) = process remaining modules (low, high + 1)
handleFlipFlop Low name on out remaining modules (low, high) = process (remaining ++ outPulse) adjustedModules (low + 1, high)
    where adjustedModules = M.adjust (\(FlipFlop on out) -> FlipFlop (not on) out) name modules
          outPulse = map (\o -> (name, (o, if on then Low else High))) out

handleConjunction :: Pulse -> String -> String -> [Element] -> [String] -> [(String, Element)] -> M.Map String Module -> (Int, Int) -> (M.Map String Module, (Int, Int))
handleConjunction pulse prev name memory out remaining modules (low, high) = process (remaining ++ outPulse) adjustedModules nextCount
    where nextCount = if pulse == High then (low, high + 1) else (low + 1, high)
          adjustedMemory = map (\m@(name, _) -> if name == prev then (prev, pulse) else m) memory
          adjustedModules = M.adjust (\(Conjunction _ out) -> Conjunction adjustedMemory out) name modules
          isNextPulseLow = all ((== High) . snd) adjustedMemory
          outPulse = map (\o -> (name, (o, if isNextPulseLow then Low else High))) out

getOutputs :: Module -> [String]
getOutputs (Broadcaster out) = out
getOutputs (FlipFlop _ out) = out
getOutputs (Conjunction _ out) = out

setConjuntionInputs :: [(String, Module)] -> [(String, [String])] -> [(String, Module)]
setConjuntionInputs [] _ = []
setConjuntionInputs ((name, Conjunction _ out):ms) modules = (name, Conjunction inputs out) : setConjuntionInputs ms modules
    where inputs = map (\(name, _) -> (name, Low)) $ filter ((name `elem`) . snd) modules
setConjuntionInputs (m:ms) modules = m : setConjuntionInputs ms modules

parseModule :: String -> (String, Module)
parseModule s = case namePart of
    ('%':name) -> (name, FlipFlop False outputs)
    ('&':name) -> (name, Conjunction [] outputs)
    _          -> (namePart, Broadcaster outputs)
    where [namePart, outPart] = splitOn " -> " s
          outputs = splitOn ", " outPart
