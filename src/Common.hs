module Common (
    pTon
  , nTop
  , kmers
  , defaultMainWith
  , genWindowEnds
  ) where

import Data.Maybe
import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Prelude hiding ( last, init, null, take, drop, length, lines, words
                      , unwords, readFile, writeFile)
import System.Environment (getArgs)

symbolToNumber :: Char -> Int
symbolToNumber c = case c of
                     'A' -> 0
                     'C' -> 1
                     'G' -> 2
                     'T' -> 3
                     _  -> error "Unknown symbol"

numberToSymbol :: Int -> Char
numberToSymbol c = case c of
                     0 -> 'A'
                     1 -> 'C'
                     2 -> 'G'
                     3 -> 'T'
                     _  -> error "Unknown number"

pTon :: ByteString -> Int
pTon bs = foldl' (\acm c -> 4 * acm + symbolToNumber c) 0 bs
{-# INLINABLE pTon #-}

nTop :: Int -> Int -> ByteString
nTop i' k' = toStrict $ toLazyByteString $ go i' k'
  where go i 1 = char8 $ numberToSymbol i
        go i k = go prefixIndex (k - 1) <> char8 (numberToSymbol r)
          where (prefixIndex,r) = quotRem i 4

kmers :: Int -> ByteString -> [Int]
kmers k bs
  | length bs < k = []
  | otherwise     = pTon (take k bs) : kmers k (drop k bs)

defaultMainWith :: (ByteString -> Int -> Int -> Int -> [ByteString]) -> IO ()
defaultMainWith fcp = do
  (inputFilename:outputFilename:_) <- getArgs
  inputFileContents <- readFile inputFilename
  let (inputString:klt:_) = lines inputFileContents
      (k:l:t:_) = catMaybes $ Prelude.map (\w -> fmap fst (readInt w)) $ words klt
  writeFile outputFilename $ unwords $ fcp inputString k l t

genWindowEnds :: ByteString -> Int -> Int -> [(Int,Int)]
genWindowEnds bs l k
  | length bs < l = []
  | otherwise     = (pTon (take k bs), pTon (drop (l - k) (take l bs)))
                      : genWindowEnds (drop l bs) l k

{-
fcp' :: ByteString -> Int -> Int -> Int -> [ByteString]
fcp' s k l t
  | ls < l    = []
  | otherwise = allMoreFreqThan k t freqMap ++ go freqMap 0 id
    where
      lastKmer i  = take k $ drop (i + 1 + l - k) s
      firstKmer i = take k $ drop i s
      ls          = length s
      freqMap     = genFreqMap k $ take l s

      go _ sc cnt | sc >= ls - l = cnt []
      go fv sc cnt
        | fv V.! ln == t - 1 = go (adjustWith ln fn fv) (sc + 1) (cnt . (lkm:))
        | otherwise = go (adjustWith ln fn fv) (sc + 1) cnt
            where lkm = lastKmer sc
                  ln  = patternToNumber lkm
                  fn  = patternToNumber (firstKmer sc)

fcp :: ByteString -> Int -> Int -> Int -> [ByteString]
fcp s k l t
  | length s < l = []
  | otherwise    = snd (Prelude.foldl (go t) (freqMap, allMoreFreqThan k t freqMap) [(patternToNumber $ take k $ drop i s, patternToNumber $ take k $ drop (i + 1 + l - k) s) | i <- [0..(length s - l - 1)]])
    where
      freqMap = genFreqMap k $ take l s

      go mf (!fv,!res) (!fp,!lp)
        | adjusted V.! lp == mf = (adjusted, (numberToPattern k lp) : res)
        | otherwise                               = (adjusted, res)
            where adjusted = adjustWith lp fp fv
-}
