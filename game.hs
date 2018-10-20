import Data.Array
import Data.Maybe

data Action = Write String

data State = State {playerno :: Int, playing :: Int, story :: [String]}

act :: State -> (Int, Action) -> State
act s (plr, Write s)
 | playing s != plr = s
 | otherwise         = State (playerno s) ((playing s + 1) `mod` playerno s) (s:(story s))