module Fibonacci.Recurrence (fib) where

-- There are some recurrence relations for the Fibonacci and Lucas numbers:
--   fib(2n) = fib(n) * luc(n)
--   fib(2n + 1) = luc(2n+1) - 2 * fib(n) * luc(n)
--   luc(2n) = 5 * (fib(n))^2 + 2*(-1)^n
--   luc(2n+1) = (5/2) * fib(n) * (fib(n)+luc(n)) + (-1)^n
-- These allow fibonacci numbers to be computed with O(log n) function evaluations.

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n
  | even n = (fib x0) * (luc x0)
  | otherwise = (luc n) - 2 * (fib x1) * (luc x1) 
  where
    x0 = n `div` 2
    x1 = (n - 1) `div` 2

square = \x -> x * x

luc :: Int -> Integer
luc 0 = 2
luc 1 = 1
luc 2 = 3
luc n
  | even n = 5 * square(fib x0) + 2 * (neg1pow x0)
  | otherwise = 5 * (f1 * (f1 + (luc x1))) `div` 2 + neg1pow x1
  where
    x0 = n `div` 2
    x1 = (n - 1) `div` 2
    f1 = fib x1
    neg1pow n
      | even n = 1
      | otherwise = -1