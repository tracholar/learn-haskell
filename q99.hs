-- 99 questions
-- https://www.haskell.org/haskellwiki/99_questions/1_to_10

myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "empty lists"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty"
elementAt x 1 = head x
elementAt (x:xs) k 
	| k<1 = error "out of bounds"
	| otherwise = elementAt xs (k-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ (x : [])

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome . tail . reverse $ xs)


data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x


compress :: (Eq a) => [a] -> [a]
compress = foldr (\x y -> if (x `elem` y) then y else x:y) []


pack :: Eq a => [a] -> [[a]]
pack = foldl f [[]]
	where 
		f [[]] x = [[x]]
		f r x = if (last r !! 0 == x)
				then take (length r - 1) r ++ [x:(last r)]
				else r ++ [[x]]


encode :: Eq a => [a] -> [(Int, a)]
encode = map f . pack
	where 
		f x = (length x, x!!0)

data CodeElem a = Single a | Multiple Int a deriving(Show,Eq)
encodeModified :: Eq a => [a] -> [CodeElem a]
encodeModified = map f . pack
	where 
		f x = if (length x == 1)
				then Single $ head x
				else Multiple  (length x) (head x)

decodeModified :: Eq a => [CodeElem a] -> [a]
decodeModified t = foldl (++) [] (map f t)
	where 
		f (Single x) = [x]
		f (Multiple s x) = replicate s x
		
dupli :: [a] -> [a]
dupli = foldl (\x y -> x ++ [y,y]) []

repli :: [a] -> Int -> [a]
repli t n = foldl (\x y -> x ++ (replicate n y)) [] t

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list n = (take (n-1) list) ++ dropEvery (drop n list) n



split :: [a] -> Int -> ([a],[a])
split x n = (take n x, drop n x)

slice :: [a] -> Int -> Int -> [a]
slice x n m = take (m-n+1) $ drop (n-1) x

rotate :: [a] -> Int -> [a]
rotate x n = if n>0
			then (drop n x) ++ (take n x)
			else (drop m x) ++ (take m x)
			where m = length x + n


removeAt :: Int -> [a] -> [a]
removeAt n list = (take (n-1) list) ++ (drop n list)


--- 31 to 41
isPrime :: Int -> Bool
isPrime x = ([]==[y| y<-[2..floor(sqrt (fromIntegral x))], mod x y==0])

myGCD :: Int -> Int -> Int
myGCD a b = if mod a b ==0
			then b
			else myGCD b (mod a b)

coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1


totient :: Int -> Int
totient n = length $ filter (coprime n) [1..n]


primeFactors :: Int -> [Int]
primeFactors n = pf n 2
	where 
		pf 1 _ = []
		pf n f
			| mod n f ==0 = f : pf (n `div` f) f
			| otherwise = pf n (f+1)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = (map f) . encode . primeFactors
	where f (x,y) = (y,x)

-- 欧拉函数
phi :: Int -> Int
phi = foldl f 1 . primeFactorsMult
	where f y x = y * (p-1) * p ^ (m-1)
		where
			p = fst x
			m = snd x

primesR :: Int -> Int -> [Int]
primesR a b = [x|x<-[a..b], isPrime x]

-- 哥德巴赫猜想
goldbach :: Int -> [(Int, Int)]
goldbach n = filter (/=(0,0)) $ map f $ primesR 2 (n `div` 2)
	where f x = if isPrime x && isPrime (n-x)
				then (x, n-x)
				else (0,0)

goldbach' = head . goldbach

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList a b = map (\x->goldbach' x) $ filter even [a..b]


goldbachList' a b c = map (\x->head x) $ filter (\x->x/=[]) arr
	where arr = map (\x->filter (\y->fst y>c && snd y>c) x) $ map (\x->goldbach x) $ filter even [a..b]
		

-- 46->50
table :: (Bool -> Bool -> Bool) -> IO()
table f = putStrLn $ concatMap (++ "\n") [show x ++ "\t" ++ show y ++ "\t" ++ show y|x<-[True,False],y<-[True,False],z<-[f x y]]


-- gray codes
gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0':) xs ++ map ('1':) (reverse xs)
	where xs = gray (n-1)

-- huffman codes
-- huffman :: [Char, Int] -> [Char, String]






