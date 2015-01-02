
data Expression
data Operator = Plus | Minus | Times | Div
data Token = TokOp Operator
			| TokIdent String
			| TokNum Int
	deriving (Show, Eq)
	

showContent :: Token -> String
showContent (TokOp op) = opToChar op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

token :: Token
token = TokIdent "x"


opToChar :: Operator -> Char
opToChar Plus = '+'
opToChar Minus = '-'
opToChar Times = '*'
opToChar Div = '/'


tokenize :: String -> [Token]
tokenize = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO()
main = do
	line <- getLine
	putStrLn line
	main