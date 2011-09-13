module Fibonacci.Extension (fib) where

import Data.Bits

-- Binet's formula for the nth Fibonacci number:
--   fib n = (phi^n - phi^(-n)) / sqrt(5), where phi = (1 + sqrt(5))/2.0.
-- One could approximate fib n by round(pow(phi, n) / sqrt(5)),
--   but for large n (say > 70), this yields incorrect results when using floats,
--   and arbitrary-precision floating-point arithmetic would be slow.
-- Another option is to use the ring Z[sqrt(5)] = {a + b * sqrt(5) | a, b are ints}.
-- These allow fibonacci numbers to be computed using powers of two-parameter numbers.
-- These exponents can be performed using O(log n) function evaluations.

-- Note: Performance is not improved by replacing Zsqrt5 with ordered pairs.

infix 6 :+  -- Operator borrowed from Data.Complex

data Zsqrt5 = Integer :+ Integer deriving (Eq)

intPart, sqrt5Part :: Zsqrt5 -> Integer
intPart (a :+ b) = a
sqrt5Part (a :+ b) = b

instance Num (Zsqrt5) where
  (x :+ y) + (c :+ d) = (x + c) :+ (y + d)
  (x :+ y) - (c :+ d) = (x - c) :+ (y - d)
  (x :+ y) * (c :+ d) = (x*c + 5*y*d) :+ (x*d + y*c)
  negate (a :+ b) = (negate a) :+ (negate b)
  abs z5 =  0             -- Note: Hack for unused function
  fromInteger n = 0 :+ 0  -- Note: Hack for unused function
  signum z5 = 0           -- Note: Hack for unused function

instance Show (Zsqrt5) where
  show (a :+ b) = show a ++ " + " ++ show b ++ " * sqrt(5)"

square = \x -> x * x

(***) :: Zsqrt5 -> Int -> Zsqrt5
z5 *** n
  | n < 0  = error "unsupported: Element in Zsqrt5 raised to a negative exponent"
  | n == 0 = 1 :+ 0
  | n == 1 = z5
  | even n = square (z5 *** (n `div` 2))
  | otherwise = z5 * (square (z5 *** (n `div` 2)))

div_sqrt5 :: Zsqrt5 -> Zsqrt5
div_sqrt5 (a :+ b) = b :+ (a `div` 5)

fib :: Int -> Integer
fib n = (intPart z5) `shiftR` n
  where
    two_phi = 1 :+ 1
    z5 = div_sqrt5 $ (2 :+ 0) * (two_phi *** n) - (2 :+ 0)