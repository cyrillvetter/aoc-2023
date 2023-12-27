import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import qualified Data.Map as M

data Pulse = High | Low deriving (Enum, Eq, Show)
data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction [Element] [String] deriving (Show)
type Element = (String, Pulse)
type ModuleMap = M.Map String Module

start = [("button", ("broadcaster", Low))]

main = do
    input <- map parseModule . lines <$> readFile "inputs/20.txt"
    let modules = M.fromList $ setConjuntionInputs input $ map (second getOutputs) input
    print $ simulatePart1 start modules (0, 0) 1
    print $ part2 modules

simulatePart1 :: [(String, Element)] -> ModuleMap -> (Int, Int) -> Int -> Int
simulatePart1 [] _ (low, high) 1000 = low * high
simulatePart1 [] modules pulseCount count = simulatePart1 start modules pulseCount (count + 1)
simulatePart1 ((src, (dest, pulse)):rest) modules (low, high) count = case element of

    Just (Broadcaster out) -> simulatePart1 (rest ++ next pulse out) modules nextPulseCount count

    Just (FlipFlop on out)
        | pulse == High -> simulatePart1 rest modules nextPulseCount count
        | otherwise -> simulatePart1 (rest ++ next nextPulse out) adjustedModules nextPulseCount count
        where adjustedModules = M.adjust (\(FlipFlop on out) -> FlipFlop (not on) out) dest modules
              nextPulse = if on then Low else High

    Just (Conjunction memory out) -> simulatePart1 (rest ++ next nextPulse out) adjustedModules nextPulseCount count
        where adjustedMemory = map (\m@(name, _) -> if name == src then (src, pulse) else m) memory
              adjustedModules = M.adjust (\(Conjunction _ out) -> Conjunction adjustedMemory out) dest modules
              nextPulse = if all ((== High) . snd) adjustedMemory then Low else High

    _ -> simulatePart1 rest modules nextPulseCount count

    where element = dest `M.lookup` modules
          nextPulseCount = if pulse == High then (low, high + 1) else (low + 1, high)
          next pulse = map (\name -> (dest, (name, pulse)))

part2 :: ModuleMap -> Int
part2 modules = foldl1 lcm $ map (\(name, _) -> simulatePart2 start name modules 1) inputs
    where (_, Conjunction inputs _) = head $ filter (\(_, m) -> "rx" `elem` getOutputs m) $ M.assocs modules

simulatePart2 :: [(String, Element)] -> String -> ModuleMap -> Int -> Int
simulatePart2 [] target modules count = simulatePart2 start target modules (count + 1)
simulatePart2 ((src, (dest, pulse)):rest) target modules count = case element of

    Just (Broadcaster out) -> simulatePart2 (rest ++ next pulse out) target modules count

    Just (FlipFlop on out)
        | pulse == High -> simulatePart2 rest target modules count
        | otherwise -> simulatePart2 (rest ++ next nextPulse out) target adjustedModules count
        where adjustedModules = M.adjust (\(FlipFlop on out) -> FlipFlop (not on) out) dest modules
              nextPulse = if on then Low else High

    Just (Conjunction memory out)
        | dest == target && nextPulse == High -> count
        | otherwise -> simulatePart2 (rest ++ next nextPulse out) target adjustedModules count
        where adjustedMemory = map (\m@(name, _) -> if name == src then (src, pulse) else m) memory
              adjustedModules = M.adjust (\(Conjunction _ out) -> Conjunction adjustedMemory out) dest modules
              nextPulse = if all ((== High) . snd) adjustedMemory then Low else High

    _ -> simulatePart2 rest target modules count

    where element = dest `M.lookup` modules
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