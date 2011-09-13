module Main where

import Control.Monad
import Ix

import Criterion.Main

import Fibonacci.Matrix     as FibMatrix
import Fibonacci.Recurrence as FibRecur
import Fibonacci.Extension  as FibExt

fib_print :: (Int -> Integer) -> Int -> String -> IO()
fib_print fib n header = putStrLn $ header ++ "fib " ++ show(n) ++ " = " ++ show(fib n)

fib_naive 0 = 0
fib_naive 1 = 1
fib_naive n = fib_naive (n - 1) + fib_naive (n - 2)

verifyN n = (p == q && r == s && p == r)
  where
    p = fib_naive     n
    q = FibRecur.fib  n
    r = FibMatrix.fib n
    s = FibExt.fib    n

verifyRange = and . map verifyN . range

verifyPrint (a, b)
    | isVerified = putStrLn $ "Fibonacci test succeeded for range " ++ rangeStr ++ "."
    | otherwise  = error $ "Fibonacci test failed for range " ++ rangeStr ++ "."
  where
    isVerified = verifyRange (a, b)
    rangeStr = show(a) ++ " -> " ++ show(b)

main = do
  -- verifyPrint (0, 10)
  defaultMain [
    bgroup "Fib"
      [ bench "FibExt.fib    1000000" $ nf FibExt.fib    1000000
      , bench "FibMatrix.fib 1000000" $ nf FibMatrix.fib 1000000
      , bench "FibRecur.fib  1000000" $ nf FibRecur.fib  1000000   
      ]
    ]