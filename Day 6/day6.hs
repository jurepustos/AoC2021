import Control.Applicative ((<$>))
import Control.Arrow (Arrow((***),second), (>>>))

splitBy :: Char -> String -> [String]
splitBy delimiter = reverse . fst . foldl (splitAux delimiter) ([],"") . (++ [delimiter])
    where
        splitAux :: Char -> ([String], String) -> Char -> ([String], String)
        splitAux delim (list,word) char = 
            if char == delim
            then (reverse word:list,"")
            else (list,char:word)

readInput :: String -> [Int]
readInput = map read . splitBy ','

type Counter = [Int]

emptyCounter :: Counter
emptyCounter = [0,0,0,0,0,0,0,0,0]

incrementCounter :: Int -> Counter -> Counter
incrementCounter _ [] = []
incrementCounter i [x] = [if i == 0 then x+1 else x]
incrementCounter i (x:xs) = 
    if i == 0 
    then (x+1):xs 
    else x:incrementCounter (i-1) xs

makeCounter :: [Int] -> Counter
makeCounter = foldl (flip incrementCounter) emptyCounter

step :: Counter -> Counter
step counter = [counter !! 1, counter !! 2, counter !! 3, counter !! 4, counter !! 5, counter !! 6, counter !! 7 + head counter, counter !! 8, head counter]

growth :: Int -> Counter -> Counter
growth r counter = 
    if r == 0
    then counter
    else growth (r-1) $ step counter

measure :: Int -> [Int] -> Int
measure rounds = sum . growth rounds . makeCounter

part1 :: [Int] -> Int
part1 = measure 80

part2 :: [Int] -> Int
part2 = measure 256

main :: IO ()
main = do
    numbers <- readInput <$> readFile "input.txt"
    print $ part1 numbers
    print $ part2 numbers