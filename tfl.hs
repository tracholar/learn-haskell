import Control.Monad

	
data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
	show Red = "red light"
	show _ = "^_^"
	
class YesNo a where
	yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
	yesno = id

instance YesNo TrafficLight where
	yesno Red = False
	yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yR nR = 
	if yesno yesnoVal then yR else nR

	
data Tree a = Node a (Tree a) (Tree a) | EmptyTree deriving(Show, Read, Eq)

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x leftsub rightsub) = 
		Node (f x) (fmap f leftsub) (fmap f rightsub)


foo :: Maybe String
foo = do
	x <- Just 3
	y <- Just "!"
	Just (show x ++ y)

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
	(c',r') <-[(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)   
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)   
                ]
	guard (c' `elem` [1..8] && r' `elem` [1..8])
	return (c',r')

