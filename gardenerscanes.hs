import Data.List (sort)

generator1 :: [(Int, Int, Int, Int)]
generator1 = [(a, 98-a, c, 98-c) | a <- [1..49], c <- [1..49]]

integerRoot :: Integer -> Integer
integerRoot n = search 0 (n+1)
  where
    search lo hi
      | hi-lo <= 1 = lo
      | mid*mid <= n = search mid hi
      | otherwise = search lo mid
      where mid = (lo+hi) `div` 2

area16 :: [Int] -> Integer
area16 sides = (a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c)
  where [a,b,c] = map toInteger sides

wholeArea :: [Int] -> Maybe Integer
wholeArea sides
  | minimum sides <= 0 || x+y <= z = Nothing
  | root*root == squared && root `mod` 4 == 0 = Just (root `div` 4)
  | otherwise = Nothing
  where
    [x,y,z] = sort sides
    squared = area16 sides
    root = integerRoot squared

selector1 :: (Int, Int, Int, Int) -> Bool
selector1 (a,b,c,d)
  | any (<=0) [a,b,c,d] = False
  | a+b /= 98 || c+d /= 98 = False
  | otherwise = case (wholeArea [small,large,70], wholeArea [middle1,middle2,70]) of
      (Just x, Just y) -> abs (x-y) == 504
      _ -> False
  where [small,middle1,middle2,large] = sort [a,b,c,d]

main :: IO ()
main = print (filter selector1 generator1)
