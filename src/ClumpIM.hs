{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Control.DeepSeq
import Common
import Data.ByteString.Char8 (ByteString, take, length, drop)
import Data.IntMap.Strict hiding (map, foldl, foldl')
import Data.List (foldl')
import Prelude hiding (length, take, filter, lookup, drop)


genFreqMap :: Int -> ByteString -> IntMap Int
genFreqMap = (foldl' (\m p -> insertWith (+) p 1 m) empty .). kmers

allMoreFreqThan :: Int -> Int -> IntMap Int -> [ByteString]
allMoreFreqThan k t = map ((flip nTop) k) . keys . filter (>=t)

adjustWith :: Int -> Int -> IntMap Int -> IntMap Int
adjustWith ln fn m = insertWith (+) ln 1 $ adjust ((-)1) fn m

fcp :: ByteString -> Int -> Int -> Int -> [ByteString]
fcp s k l t
  | length s < l = []                                      
  | otherwise    = snd $ foldl' (go t) (freqMap, allMoreFreqThan k t freqMap) $ genWindowEnds s l k
    where
      freqMap     = genFreqMap k $ take l s

      go t' (fm,acm) (fp,lp)
        | Just lv <- lookup lp fm
        , lv == t' - 1 = (adjustWith lp fp fm, (nTop k lp):acm)
        | otherwise    = (adjustWith lp fp fm, acm)

main :: IO ()
main = defaultMainWith fcp
