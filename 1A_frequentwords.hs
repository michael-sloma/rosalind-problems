import qualified Data.HashMap.Strict as HM  
type FreqMap = HM.HashMap String Integer
--generate all sliding windows of length n in text
windows::Int->String->[String]
windows 0 _ = []
windows n text | length (take n text) >= n = take n text:windows n (drop 1 text)
               | otherwise = []

--from a string and a window size, compute frequency of each unique window
frequencies::Int->String->FreqMap
frequencies n s = foldr step HM.empty (windows n s)
    where step x = HM.insertWith (+) x 1

--extract the most frequent strings from a map of strings
mostFrequent::FreqMap->[String]
mostFrequent hm = HM.keys best
    where best = HM.filter (== maximum (HM.elems hm)) hm

--find the most frequent kmers of length k in string text
mostFrequentKmer::Int->String->[String]
mostFrequentKmer k text = mostFrequent (frequencies k text)

prettify::[String]->String
prettify = foldr concatWithSpace []
    where concatWithSpace a b = a++" "++b

main::IO()
main = do 
    text <- getLine
    k <- getLine
    putStrLn $ prettify $ mostFrequentKmer (read k) text