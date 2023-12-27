import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import qualified Data.Map as M

data Pulse = High | Low deriving (Enum, Eq, Show)
data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction [Element] [String] deriving (Show)
type Element = (String, Pulse)
type ModuleMap = M.Map String Module

repetitions = 1000

main = do
    input <- map parseModule . lines <$> readFile "inputs/20.txt"
    let modules = M.fromList $ setConjuntionInputs input $ map (second getOutputs) input
    print $ repeatProcess modules

repeatProcess :: ModuleMap -> Int
repeatProcess modules = low * high
    where (low, high) = snd $ iterate (uncurry (process [("button", ("broadcaster", Low))])) (modules, (0, 0)) !! repetitions

process :: [(String, Element)] -> ModuleMap -> (Int, Int) -> (ModuleMap, (Int, Int))
process [] modules count = (modules, count)
process ((src, (dest, pulse)):rest) modules (low, high) = case element of

    Just (Broadcaster out) -> process (rest ++ next pulse out) modules nextCount

    Just (FlipFlop on out)
        | pulse == High -> process rest modules nextCount
        | otherwise -> process (rest ++ next nextPulse out) adjustedModules nextCount
        where adjustedModules = M.adjust (\(FlipFlop on out) -> FlipFlop (not on) out) dest modules
              nextPulse = if on then Low else High

    Just (Conjunction memory out) -> process (rest ++ next nextPulse out) adjustedModules nextCount
        where adjustedMemory = map (\m@(name, _) -> if name == src then (src, pulse) else m) memory
              adjustedModules = M.adjust (\(Conjunction _ out) -> Conjunction adjustedMemory out) dest modules
              nextPulse = if all ((== High) . snd) adjustedMemory then Low else High

    _ -> process rest modules nextCount

    where element = dest `M.lookup` modules
          nextCount = if pulse == High then (low, high + 1) else (low + 1, high)
          next pulse = map (\name -> (dest, (name, pulse)))

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