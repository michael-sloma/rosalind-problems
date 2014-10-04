import Data.List
import qualified Data.HashMap.Strict as HM  
import qualified Data.HashSet as HS 
type FreqMap = HM.HashMap String Int

--generate all sliding windows of length n in text
windows::Int->String->[String]
windows n text = filter (\x->length x == n) $ map (take n) $ (init . tails) text

--from a string and a window size, compute frequency of each unique window
frequencies::Int->String->FreqMap
frequencies n s = foldr step HM.empty (windows n s)
    where step x hm = (HM.insertWith (+) x 1 hm) 

--from a freqmap and a new overlapping window, generate the new freqmap
updateFrequencies::(Int,String,FreqMap)->String->(Int,String,FreqMap)
updateFrequencies (k,old,hm) new = (k,new,(addKey inn . delKey out) hm)
    where out = take k old
          inn = reverse $ take k $ reverse new --get last k chars
          delKey key = HM.insertWith (-) key 1
          addKey key = HM.insertWith (+) key 1

--scan across the list of windows, updating hashmap of kmers as we go
chunkFrequencies::Int->Int->String->[FreqMap]
chunkFrequencies chnksz k txt = map third $ scanl updateFrequencies initial wndws
    where initial = (k,take chnksz txt,frequencies k $ take chnksz txt)
          wndws = tail $ windows chnksz txt
          third (_,_,x) = x

--extract the most frequent strings from a map of strings
extractClumps::Int->FreqMap->[String]
extractClumps t hm = HM.keys $ HM.filter (>= t) hm

--find the most frequent kmers of length k in string text
clumpsInChunk::Int->Int->String->[String]
clumpsInChunk t k text = extractClumps t (frequencies k text)

--find list of all strings of length k that form a (chnksz,t) clump in text
clumpFinding::Int->Int->Int->String->[String]
clumpFinding chnkSz t k text = removeDuplicates $ concatMap (clumpsInChunk t k) (windows chnkSz text)
    where removeDuplicates xs = HS.toList $ HS.fromList xs

clumpFinding'::Int->Int->Int->String->[String]
clumpFinding' chnkSz t k text = removeDuplicates $  concatMap (extractClumps t) (chunkFrequencies chnkSz k text)
    where removeDuplicates xs = HS.toList $ HS.fromList xs


prettify::[String]->String
prettify = foldr concatWithSpace []
    where concatWithSpace a b = a++" "++b

main::IO()
main = do 
    genome <- getLine
    k <- getLine
    chunksize <- getLine
    repeats <- getLine
    --putStrLn $ prettify $ clumpFinding (read chunksize) (read repeats) (read k) genome
    putStrLn $ prettify $ clumpFinding' (read chunksize) (read repeats) (read k) genome
