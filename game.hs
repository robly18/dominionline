import Data.Array
import Data.Maybe

data Effect = Money Int | Draw Int

type Card = (String, Effect)
type Deck = [Card]


type Hand = (Deck, Deck, Deck) --hand, draw, discard
type Hands = Array Int Hand

type Board = (Int, Hands, Array Int (Card, Int))

data Action = Pass | Play Int

drawOne :: Hand -> Hand --for now, no shuffling. i need to implement that though.
drawOne (hand, [], []) = (hand, [], [])
drawOne (hand, [], discard) = drawOne (hand, discard, [])
drawOne (hand, d:draw, discard) = (d:hand, draw, discard)

draw :: Int -> Int -> Hands -> Hands --draw howmany playerno hands
draw n plr hands = let hand = hands!plr in
                    let newhand = (iterate drawOne hand) !! n in
                    hands // [(plr, newhand)]

fetch :: Int -> [a] -> Maybe (a, [a])
fetch _ [] = Nothing
fetch 0 (x:xs) = Just (x, xs)
fetch n (x:xs) = case fetch n xs of Nothing -> Nothing
                                    Just (y, ys) -> Just (y, x:ys)

{-
play :: Board -> Maybe Int -> Board --Just x means card x is played, Nothing means end turn, go to next
play (plr, decks, board) Nothing = let plrno = snd $ bounds decks in
                            ((plr + 1) `mod` plrno, draw 5 plr decks, board)
play b@(plr, decks, board) (Just n) = let (hand, draw, discard) = decks ! plr in do
                                        (card, newhand) <- fetch n hand-}

nextPlayer :: Board -> Board
nextPlayer (plr, decks, board) = let plrno = snd $ bounds decks in ((plr+1) `mod` plrno, draw 5 plr decks, board)

playCard :: Int -> Board -> Board
playCard c b@(plr, decks, board) =  fromMaybe b $ do
                                        let (hand, draw, discard) = decks ! plr
                                        (card, newhand) <- fetch c hand
                                        let newdeck = (newhand, draw, card:discard)
                                        let newdecks = decks//[(plr, newdeck)]
                                        return (plr, newdecks, board)

act :: Board -> Action -> Board
act b@(plr, decks, board) act = case act of Pass   -> nextPlayer b
                                            Play c -> playCard c b

