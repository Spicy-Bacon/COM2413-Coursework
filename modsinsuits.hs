import Data.List (nub, permutations)
import Data.Ratio ((%), denominator)

type Card = (Char, Int)
type Hand = (Card, Card)
type Game = (Hand, Hand)

cards :: [Card]
cards = [(s,n) | s <- "CDHS", n <- [1..13]]

modified :: Card -> Card -> Rational
modified ('S',n) _ = fromIntegral (n*n)
modified ('H',n) _ = fromIntegral n % 2
modified ('C',n) _ = fromIntegral (-n)
modified ('D',n) (_,other) = fromIntegral n % fromIntegral other
modified _ _ = 0

score :: Hand -> Rational
score (a,b)
  | snd a == snd b = 0
  | otherwise = modified a b + modified b a

-- Store each hand's score once, rather than repeatedly calculating it.
scoredHands :: Char -> Char -> [(Hand, Rational)]
scoredHands s t =
  [(hand, score hand) | a <- [1..13], b <- [1..13], let hand = ((s,a),(t,b))]

generator3 :: [Game]
generator3 =
  [(jo,bo)
  | [s,t,u,v] <- permutations "CDHS"
  , let boHands = scoredHands u v
  , (jo,j) <- scoredHands s t, j > 0, denominator j == 1
  , (bo,b) <- boHands, b == -j
  ]

validCard :: Card -> Bool
validCard (s,n) = s `elem` "CDHS" && n >= 1 && n <= 13

selector3 :: Game -> Bool
selector3 (jo@(j1,j2),bo@(b1,b2)) =
  all validCard dealt
  && length (nub (map fst dealt)) == 4
  && length (nub (map snd dealt)) == 4
  && j > 0 && denominator j == 1 && score bo == -j
  && or [True
        | jNew <- remaining
        , score (j1,jNew) == j
        , bNew <- remaining, bNew /= jNew
        , length (nub (map fst [j1,jNew,b1,bNew])) == 4
        , score (b1,bNew) == score bo
        ]
  where
    dealt = [j1,j2,b1,b2]
    remaining = filter (`notElem` dealt) cards
    j = score jo

-- These are initial games; selector3 proves suitable exchanges exist.
main :: IO ()
main = print (filter selector3 generator3)
