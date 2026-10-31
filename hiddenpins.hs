import Data.Char (isDigit)

-- Candidates need not yet satisfy the square relation: that is selector2's job.
generator2 :: [(String, String)]
generator2 =
  [(card, telephone)
  | a <- ['1'..'9'], b <- ['0'..'9'], b < a
  , c <- ['0'..'9'], c < b
  , d <- ['0'..'9'], d < c
  , let card = [a,b,c,d]
  , let available = filter (`notElem` card) ['0'..'9']
  , w <- available, w /= '0'
  , x <- available, y <- available, z <- available
  , let telephone = [w,x,y,z]
  ]

-- Preconditions about decreasing and disjoint digits are handled by generator2.
selector2 :: (String, String) -> Bool
selector2 (card, telephone)
  | not (fourDigits card && fourDigits telephone) = False
  | otherwise = root*root == (read telephone :: Int)
  where
    root = read (drop 2 card) :: Int
    fourDigits s = length s == 4 && all isDigit s && head s /= '0'

-- The repeated-digit clue distinguishes one of the three printed candidate pairs.
main :: IO ()
main = print (filter selector2 generator2)
