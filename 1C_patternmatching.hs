import Data.List

windows::Int->String->[String]
windows 0 _ = []
windows n text | length (take n text) >= n = take n text:windows n (drop 1 text)
               | otherwise = []

patternMatching::String->String->[Int]
patternMatching gen pat = reverse . snd $ 
							foldl' step (0,[]) $ windows (length pat) gen
	where step (ind,acc) wndw = (ind+1,if wndw==pat then ind:acc else acc)

prettify::[String]->String
prettify = foldr concatWithSpace []
    where concatWithSpace a b = a++" "++b

main::IO()
main = do 
    pattern <- getLine
    genome <- getLine
    putStrLn $ prettify $ map show $ patternMatching genome pattern