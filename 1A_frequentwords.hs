import qualified Data.HashMap.Lazy as HM  

--generate all sliding windows of length n in text
windows::Int->String->[String]
windows 0 _ = []
windows n text | length (take n text) >= n = take n text:(windows n $ drop 1 text)
               | otherwise = []

--from a string and a window size, compute frequency of each unique window
frequencies::Int->String->(HM.HashMap String Integer)
frequencies n s = window_freq (windows n s) HM.empty
    where window_freq [] hm = hm
          window_freq (x:xs) hm = window_freq xs (HM.insertWith (+) x 1 hm) 

--extract the most frequent strings from a map of strings
mostFrequent::(HM.HashMap String Integer)->[String]
mostFrequent hm = HM.keys $ HM.filter is_best hm
              where is_best = (== maximum (HM.elems hm)) 

--find the most frequent kmers of length k in string text
mostFrequentKmer::Int->String->[String]
mostFrequentKmer k text = mostFrequent (frequencies k text)

prettify::[String]->String
prettify = foldr myconcat []
    where myconcat a b = a++" "++b

main::IO()
main = do 
    text <- getLine
    k <- getLine
    putStrLn $ prettify $ mostFrequentKmer (read k) text