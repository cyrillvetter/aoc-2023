main = do
    input <- lines <$> readFile "inputs/18.txt"
    print $ getTotalSize parseP1 input
    print $ getTotalSize parseP2 input

moveDirection :: (Int, Int) -> (Char, Int) -> (Int, Int)
moveDirection (x, y) (dir, steps)
    | dir == 'R' || dir == '0' = (x + steps, y)
    | dir == 'L' || dir == '2' = (x - steps, y)
    | dir == 'U' || dir == '3' = (x, y - steps)
    | dir == 'D' || dir == '1' = (x, y + steps)

getTotalSize :: (String -> (Char, Int)) -> [String] -> Int
getTotalSize parser input = trenchSize parsed + shoelace (scanl moveDirection (0, 0) parsed)
    where parsed = map parser input

shoelace :: [(Int, Int)] -> Int
shoelace ps = abs (sum $ zipWith (\(x1, y1) (x2, y2) -> (y1 + y2) * (x1 - x2)) (tail ps) ps) `div` 2

trenchSize :: [(Char, Int)] -> Int
trenchSize = (+ 1) . (`div` 2) . sum . map snd

parseP1 :: String -> (Char, Int)
parseP1 s = (head dir, read num)
    where (dir:num:_) = words s

parseP2 :: String -> (Char, Int)
parseP2 s = (last color, num)
    where colorPart = last $ words s
          color = drop 2 $ init colorPart
          num = (read . ("0x" ++)) $ init color
