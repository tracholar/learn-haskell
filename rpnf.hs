import Data.List


solveRPN :: (Num a) => String -> a
solveRPN = head (foldl foldingFunc [] . words )

foldingFunc :: (Num a) => [a] -> b -> [a]
foldingFunc (x:y:ys) "*" = (x*y):ys
foldingFunc (x:y:ys) "+" = (x+y):ys
foldingFunc (x:y:ys) "-" = (y-x):ys
foldingFunc xs numberString = read numberString:xs
