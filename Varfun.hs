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

	