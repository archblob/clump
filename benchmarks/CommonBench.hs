module Main(main) where

import Criterion.Main
import Data.ByteString.Char8 (ByteString, pack)
import Data.List (cycle)

import Common

main :: IO ()
main = defaultMain [
    bench "pattern to number" $ nf pTon (pack (take 10000 (cycle "ACGT")))
  , bench "kmers" $ nf (kmers 9) (pack (take 10000 (cycle "ACGT")))
  , bench "window ends" $ nf (\s -> genWindowEnds s 2 10) (pack (take 10000 (cycle "ACGT")))
  ]
