import Data.List (nub)
import qualified Data.Set as Set

isPrime :: Int -> Bool
isPrime n = n >= 2 && all (\d -> n `mod` d /= 0) [2..limit]
  where limit = floor (sqrt (fromIntegral n :: Double))

oddTriples :: [String]
oddTriples = [[a,b,c] | a <- "13579", b <- "13579", c <- "13579"]

primeTriples :: Set.Set String
primeTriples = Set.fromList [s | s <- oddTriples, isPrime (read s)]

reversiblePrimes :: Set.Set String
reversiblePrimes = Set.filter (\s -> Set.member (reverse s) primeTriples) primeTriples

generator4 :: [String]
generator4 = Set.toAscList (primeTriples `Set.difference` reversiblePrimes)

-- Indices of the three rows, three downward columns and two downward diagonals.
lineIndices :: [[Int]]
lineIndices = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]]

linesOf :: String -> [String]
linesOf grid = [map (grid !!) indices | indices <- lineIndices]

-- Build George's grid row by row, pruning impossible column prefixes early.
georgeGrids :: [String]
georgeGrids =
  [grid
  | top <- rows
  , middle <- rows, top /= middle
  , all (\i -> [top !! i, middle !! i] `Set.member` prefixes) [0..2]
  , bottom <- rows, bottom /= top, bottom /= middle
  , let grid = top ++ middle ++ bottom
  , let ns = linesOf grid
  , all (`Set.member` reversiblePrimes) ns
  , length (nub ns) == 8
  ]
  where
    rows = Set.toAscList reversiblePrimes
    prefixes = Set.fromList (map (take 2) rows)

replaceDigit :: Int -> Char -> String -> String
replaceDigit i digit grid = take i grid ++ [digit] ++ drop (i+1) grid

-- Change one square, then enforce all of Martha's conditions.
-- This shared set is computed once for all calls to selector4.
oddOnesOut :: Set.Set String
oddOnesOut = Set.fromList
  [bad
  | george <- georgeGrids
  , i <- [0..8], digit <- "13579", digit /= george !! i
  , let martha = replaceDigit i digit george
  , let ns = linesOf martha
  , all (`Set.member` primeTriples) ns
  , length (nub ns) == 8
  , let exceptions = filter (`Set.notMember` reversiblePrimes) ns
  , [bad] <- [exceptions]
  ]

selector4 :: String -> Bool
selector4 n = Set.member n oddOnesOut

main :: IO ()
main = print (filter selector4 generator4)
