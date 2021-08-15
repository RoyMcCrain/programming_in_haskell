even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

abs :: Int -> Int
abs n = if n >= 0 then n else - n

abs' :: Int -> Int
abs' n
  | n >= 0 = n
  | otherwise = - n

signum :: Int -> Int
signum n =
  if n < 0
    then -1
    else if n == 0 then 0 else 1

signum' :: Int -> Int
signum' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

const :: a -> (b -> a)
const x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n - 1]

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2
