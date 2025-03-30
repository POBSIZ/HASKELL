module Lib (parMapFib) where

import Control.Parallel.Strategies (NFData, parMap, rdeepseq)

fib :: (Integral a) => a -> a
fib n = nfib n (0, 1)
  where
    nfib 0 (a, _) = a
    nfib x (a, b) = nfib (x - 1) (b, a + b)

parMapFib :: (Integral a, NFData a) => [a] -> [a]
parMapFib = parMap rdeepseq fib