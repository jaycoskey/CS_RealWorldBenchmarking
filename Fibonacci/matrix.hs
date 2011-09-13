module Fibonacci.Matrix (fib) where

-- The nth Fibonacci number can be defined by the following recurrence relation:
-- F = B^n, where
--   the first row of F  = [F(n+1), F(n)  ]; the first row of B  = [1, 1]
--   the second row of F = [F(n),   F(n-1)]; the second row of B = [1, 0].

data Matrix22 = Matrix22 Integer Integer Integer Integer deriving (Eq, Show)

instance Num Matrix22 where
  (+) (Matrix22 a b c d) (Matrix22 a' b' c' d') = Matrix22 (a+a')(b+b')(c+c')(d+d')
  (-) (Matrix22 a b c d) (Matrix22 a' b' c' d') = Matrix22 (a-a')(b-b')(c-c')(d-d')
  (*) (Matrix22 a b c d) (Matrix22 a' b' c' d')
    = Matrix22 (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')
  negate (Matrix22 a b c d) = Matrix22 (-a) (-b) (-c) (-d)
  fromInteger a = Matrix22 a 0 0 a
  abs m = error "abs(Matrix22) is a stub, and should not be called"
  signum m = error "signum(Matrix22) is a stub, and should not be called"

get11, get12, get21, get22 :: Matrix22 -> Integer
get11 (Matrix22 a b c d) = a
get12 (Matrix22 a b c d) = b
get21 (Matrix22 a b c d) = c
get22 (Matrix22 a b c d) = d

base = Matrix22 1 1 1 0

square = \x -> x * x

powMatrix22 :: Matrix22 -> Int -> Matrix22
powMatrix22 m n
  | n == 0 = Matrix22 1 0 1 0
  | n == 1 = m
  | even n = square $ powMatrix22 m (n `div` 2)
  | odd n  = m * (square $ powMatrix22 m (n `div` 2))

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = get11 $ powMatrix22 base (n - 1)