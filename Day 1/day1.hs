import Control.Arrow ( Arrow(arr,second,(&&&)), (>>>))

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

call3 :: (a -> b) -> (a -> c) -> (a -> d) -> (a -> (b,c,d))
call3 f g h x = (f x, g x, h x) 

toInts :: [String] -> [Int]
toInts = map read

withTail :: [a] -> ([a], [a])
withTail = id &&& tail

withTail3 :: [a] -> ([a], [a], [a])
withTail3 = call3 id tail (tail . tail)

increases :: [Int] -> [Bool]
increases = uncurry (zipWith (<)) . withTail

plus3 :: Int -> Int -> Int -> Int
plus3 a b c = a+b+c

windows :: [Int] -> [Int]
windows = uncurry3 (zipWith3 plus3) . withTail3

readInput :: IO [Int]
readInput = do
    contents <- readFile "input.txt"
    let strLines = lines contents
    return $ toInts strLines

part1 :: [Int] -> Int
part1 input =
    let numbers = input in
    length $ filter id (increases numbers)

part2 :: [Int] -> Int
part2 input = 
    let numbers = input in
    length $ filter id (increases $ windows numbers)

main :: IO ()
main = do
    input <- readInput
    print $ part1 input
    print $ part2 input