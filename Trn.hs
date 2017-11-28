module Trn where

fibonacci 0 = 0
fibonacci n = fibacc 1 0 n
fibacc acc prev 0 = acc
fibacc acc prev 1 = acc
fibacc acc prev n | n > 0 = fibacc (acc + prev) acc (n - 1)
                  | n < 0 = (-1) ^ (-n + 1) * fibacc acc prev (-n)

{-
    seqA 0 = 1
    seqA 1 = 2
    seqA 2 = 3
    seqA n = seqA (n - 1) + seqA (n - 2) - 2 * seqA (n - 3)
-}
seqA n  | n < 0 = error "n must be positive"
        | n < 3 = n + 1
        | otherwise =
            let
                helper k3 _ _ 2 = k3
                helper k3 k2 k1 n = helper
                                        (k3 + k2 - 2 * k1)
                                        k3
                                        k2
                                        (n - 1)
            in helper 3 2 1 n
