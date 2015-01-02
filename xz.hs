import System.IO
import Control.Monad

main = do
	let list = []
	handle <- openFile "xz.txt" ReadMode
	contents <- hGetContents handle
	print contents
	hClose handle
	
