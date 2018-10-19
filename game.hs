import Data.Array

data Effect = Money Int | Draw Int

type Card = (String, Effect)
type Deck = [Card]

type Hands = Array Int (Deck, Deck, Deck)

type Board = (Int, Hands, Array Int (Card, Int))

drawNew :: Hands -> Int -> Hands
drawNew decks plr = --todo

play :: Board -> Maybe Int -> Board #Just x means card x is played, Nothing means end turn, go to next
play (plr, decks, board) Nothing = let plrno = snd $ bounds decks in
								((plr + 1) % plrno, decks, board)