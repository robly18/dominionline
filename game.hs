import Data.Array

data Effect = Money Int | Draw Int

type Card = (String, Effect)
type Deck = [Card]


type Hand = (Deck, Deck, Deck) --hand, draw, discard
type Hands = Array Int Hand

type Board = (Int, Hands, Array Int (Card, Int))

drawOne :: Hand -> Hand --for now, no shuffling. i need to implement that though.
drawOne (hand, [], discard) = drawOne (hand, discard, [])
drawOne (hand, d:draw, discard) = (d:hand, draw, discard)

draw :: Int -> Int -> Hands -> Hands --draw howmany playerno hands
draw n plr hands = let hand = hands!plr in
                    let newhand = (iterate drawOne hand) !! n in
                    hands // [(plr, newhand)]


play :: Board -> Maybe Int -> Board --Just x means card x is played, Nothing means end turn, go to next
play (plr, decks, board) Nothing = let plrno = snd $ bounds decks in
                            ((plr + 1) `mod` plrno, draw 5 plr decks, board)
play (plr, decks, board) (Just n) = let (hand, draw, discard) = decks ! plr in
                                       let --todo