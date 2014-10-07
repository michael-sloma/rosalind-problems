import qualified Data.Vector as V

skew::String->V.Vector Int
skew s = V.scanl step 0 (V.fromList s)
	where step i c | c=='C' = i+1
				   | c=='G' = i-1
				   | otherwise = i

maxSkew::String->V.Vector Int
maxSkew s = V.map fst $ V.filter (\x->snd x==V.maximum vs) (V.indexed vs)
	where vs = skew s

main::IO()
main = do
	text <- getLine
	V.mapM_ putStrLn $ V.map show $ maxSkew text