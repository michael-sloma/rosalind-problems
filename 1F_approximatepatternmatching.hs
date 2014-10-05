import Data.List

--generate all sliding windows of length n in text
windows::Int->String->[String]
windows n text = filter (\x->length x == n) $ map (take n) $ (init . tails) text

--compute hamming distance between two strings of equal length
hamming::String->String->Int
hamming a b = sum . map (\(x,y)->if x==y then 0 else 1) $ zip a b

--find instances of pat in gen with at most d mismatches
appPtrnMtch::Int->String->String->[Int]
appPtrnMtch d gen pat = reverse . snd $ 
							foldl' step (0,[]) $ (windows (length pat) gen)
	where step (ind,acc) wndw = (ind+1,if (hamming pat wndw) <= d 
										then ind:acc else acc)

main::IO()
main = do
	pattern<-getLine
	genome<-getLine
	mismatches<-getLine
	mapM_ putStr $ intersperse " " $ map show $ 
		appPtrnMtch (read mismatches) genome pattern
	putStrLn " "