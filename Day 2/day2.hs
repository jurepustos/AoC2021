import Data.Maybe ( fromJust,isJust )
import Control.Arrow ( Arrow((&&&)), (>>>) )

data Command = Forward Int | Up Int | Down Int

parseLine :: String -> (String,Int)
parseLine = (head &&& read . head . tail) . words

parseInput :: String -> Maybe Command
parseInput = uncurry parseCommString . parseLine

parseCommString :: String -> Int -> Maybe Command
parseCommString comStr num = 
    case comStr of
        "forward" -> Just $ Forward num
        "down" -> Just $ Down num
        "up" -> Just $ Up num
        _ -> Nothing

parseCommands :: String -> [Command]
parseCommands = map fromJust . filter isJust . map parseInput . lines

update1 :: (Int,Int) -> Command -> (Int,Int)
update1 (pos,depth) comm =
    case comm of
        Forward x -> (pos+x, depth)
        Down y -> (pos, depth+y)
        Up y -> (pos, depth-y)

update2 :: (Int,Int,Int) -> Command -> (Int,Int,Int)
update2 (pos,depth,aim) comm =
    case comm of
        Forward x -> (pos+x, depth + x*aim, aim)
        Down y -> (pos, depth, aim+y)
        Up y -> (pos, depth, aim-y)

part1 :: [Command] -> IO ()
part1 commands = do
    let (pos, depth) = foldl update1 (0,0) commands
    print $ pos * depth

part2 :: [Command] -> IO ()
part2 commands = do
    let (pos, depth, _) = foldl update2 (0,0,0) commands
    print $ pos * depth

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let commands = parseCommands contents
    part1 commands
    part2 commands
    
