r = 5.0			-- the variable x is 5.0
area t = pi * t ^ 2
{-- 
Do this just because you can
--}
double x = 2*x
quadruple x = double(double x)
square x = x * x
half x =  x / 2



-- 

areaRect l w = l * w



-- haron's formula

heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
	where
	s = (a + b + c) / 2


abs x 
	| x<0 = 0-x
	| otherwise = x
	
{--
NumSolve a b c
	| d > 0 = 2
	| d == 0 = 1
	| otherwise = 0
		where
		d = b^2 - 4*a*c
--}