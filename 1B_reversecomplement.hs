reverseComplement::String->String
reverseComplement = reverse . map translate

translate::Char->Char
translate 'A' = 'T'
translate 'T' = 'A'
translate 'C' = 'G'
translate 'G' = 'C'

main::IO()
main = do
	text<-getLine
	putStrLn $ reverseComplement text