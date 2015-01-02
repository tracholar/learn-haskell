lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY!"
lucky x = "Sorry!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

head' :: [a] -> a
head' [] = error "Error"
head' (x:_) = x


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


bmiTell :: (RealFloat a) => a -> a -> String   
bmiTell weight height   
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"   
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"   
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"   
    | otherwise                 = "You're a whale, congratulations!"    
	
	
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
	| otherwise = b

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x:replicate' (n-1) x

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
	let smallerSorted = qsort [a | a <- xs, a <= x]
	    biggerSorted = qsort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted
	
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



sum' :: (Num a) => [a] -> [a]
sum' xs = foldl (\acc x -> x:acc) [] xs

main = do
	putStrLn "Hello, your name?"
	name <- getLine
	putStrLn ("hey" ++ name)
