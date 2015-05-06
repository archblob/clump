module Main(main) where
 
import Data.List (nub)
import Data.Map hiding (foldl, null)
import Prelude  hiding (filter)
import System.Environment (getArgs)
 
kmers :: Int -> String -> [String]
kmers k xs = [take k (drop x xs) | x <- [0..(length xs - k)]]
 
allMoreFreqThan :: Int -> Map String Int -> [String]
allMoreFreqThan t = keys . filter (>=t)
 
genFreqMap :: Int -> String -> Map String Int
genFreqMap = (foldl (\m p -> insertWith (+) p 1 m) empty .). kmers
 
findClumpPatterns, findClumpPatterns' :: String -> Int -> Int -> Int -> [String]
findClumpPatterns s k l t = nub $ concatMap (allMoreFreqThan t . genFreqMap k) $ kmers l s
 
findClumpPatterns' s k l t
  | ls < l    = []
  | otherwise = allMoreFreqThan t freqMap ++ go s freqMap 0 id
    where
      ls      = length s
      freqMap = genFreqMap k $ take l s
  
      go xs _ sc cnt | null xs || sc >= ls - l = cnt []
      go (x:xs) m sc cnt = go xs (insertWith (-) fp 1 am) (sc + 1) ncnt 
        where fp   = x : take (k- 1) xs
              lp   = drop (l - k) (take l xs)
              ncnt = cnt . maybe id (\v -> if v == t - 1 then (lp:) else id) oldLpCount
              (oldLpCount, am) = insertLookupWithKey (const (+)) lp 1 m
 
main :: IO ()
main = do
  (inputFilename:outputFilename:_) <- getArgs
  inputFileContents <- readFile inputFilename
  let (inputString:klt:_) = lines inputFileContents
      (k:l:t:_) = Prelude.map read $ words klt
  writeFile outputFilename $ unwords $ findClumpPatterns' inputString k l t
  
