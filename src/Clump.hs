{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import Data.List (foldl')
import Data.ByteString.Char8 hiding (foldl)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as GM
import qualified Data.Vector.Unboxed as V
import Data.Monoid
import Prelude  hiding (filter, readFile, writeFile, take, lines
                       , words, drop, unwords, length, tail, head, null
                       , init, last, lookup)
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

patternToNumber :: ByteString -> Int
patternToNumber bs = Data.ByteString.Char8.foldl' (\acm c -> 4 * acm + symbolToNumber c) 0 bs

numberToPattern :: Int -> Int -> ByteString
numberToPattern i' k' = toStrict $ B.toLazyByteString $ go i' k'
  where go i 1 = B.char8 $ numberToSymbol i
        go i k = go prefixIndex (k - 1) <> B.char8 (numberToSymbol r)
          where (prefixIndex,r) = quotRem i 4

kmers :: Int -> ByteString -> [Int]
kmers k xs = [patternToNumber (take k (drop x xs)) | x <- [0..(length xs - k)]]

genFreqMap :: Int -> ByteString -> V.Vector Int
genFreqMap k xs = V.create (do { v <- GM.new (4^k); GM.set v 0; mapM_ (\x -> do {ov <- GM.read v x; GM.write v x (ov + 1)}) (kmers k xs) ; return v;})

allMoreFreqThan :: Int -> Int -> V.Vector Int -> [ByteString]
allMoreFreqThan k t = Prelude.map ((flip numberToPattern) k . fst) . V.toList . V.filter ((>=t) . snd) . V.indexed

adjustWith :: Int -> Int -> V.Vector Int -> V.Vector Int
adjustWith lp fp fv = fv V.// [(fp,fv V.! fp - 1),(lp, fv V.! lp + 1)]

adjustWith' :: Int -> Int -> V.Vector Int -> V.Vector Int
adjustWith' lp fp fv = V.modify (\v -> do {ofv <- GM.read v fp; olv <- GM.read v lp; GM.write v fp (if ofv <= 0 then 0 else ofv - 1); GM.write v lp (olv + 1)}) fv

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

fcp'' :: ByteString -> Int -> Int -> Int -> [ByteString]
fcp'' s k l t
  | length s < l = []
  | otherwise    = go (genFreqMap k $ take l s) [(patternToNumber $ take k $ drop i s, patternToNumber $ take k $ drop (i + 1 + l - k) s) | i <- [0..(length s - l - 1)]]
    where
      go v pm = runST $ do
          mv <- V.thaw v
          foldM (\(!acm) (!fn, !ln) -> do {ofv <- GM.read mv fn; olv <- GM.read mv ln; GM.write mv fn (if ofv <= 0 then 0 else ofv - 1); GM.write mv ln (olv + 1); return (if olv == t - 1 then numberToPattern ln k : acm else acm)}) (allMoreFreqThan k t v) pm

main :: IO ()
main = do
  (inputFilename:outputFilename:_) <- getArgs
  inputFileContents <- readFile inputFilename
  let (inputString:_) = lines inputFileContents
      -- (k:l:t:_) = catMaybes $ Prelude.map (\w -> fmap fst (readInt w)) $ words klt
  writeFile outputFilename $ unwords $ fcp inputString 9 500 5
