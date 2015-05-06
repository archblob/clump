{-# LANGUAGE BangPatterns #-}
module Main where

import Common
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString, take, length)
import qualified Data.Vector.Unboxed.Mutable as GM
import qualified Data.Vector.Unboxed as V
import Prelude  hiding (length, take)


genFreqMap :: GM.STVector s Int -> Int -> ST s ()
genFreqMap v x = GM.read v x >>= \ov -> GM.write v x (ov + 1)

allMoreFreqThan :: Int -> Int -> V.Vector Int -> [ByteString]
allMoreFreqThan k t = Prelude.map ((flip nTop) k . fst) . V.toList . V.filter ((>=t) . snd) . V.indexed

fcp :: ByteString -> Int -> Int -> Int -> [ByteString]
fcp s k l t
  | length s < l = []
  | otherwise    = go (kmers k $ take l s) (genWindowEnds s l k)
    where go ks pm = runST $ do
                       v <- GM.new (4^k)
                       GM.set v 0
                       mapM_ (genFreqMap v) ks
                       foldM (upv v t k) [] pm

upv :: GM.STVector s Int -> Int -> Int -> [ByteString] -> (Int, Int) -> ST s [ByteString]
upv !v !t !k !acm (!fn, !ln) = do
  ofv <- GM.read v fn
  let !nfn = if ofv <= 0 then 0 else ofv -1
  GM.write v fn nfn
  
  olv <- GM.read v ln
  GM.write v ln (olv + 1)
  return $ if olv == t -1 then nTop ln k : acm else acm
  
main :: IO ()
main = defaultMainWith fcp
