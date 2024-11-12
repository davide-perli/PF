myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y
          
maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = if(x >= y)
		then
			if(x >= z)
				then x
				else z
		else if(y <= z)
			then z
			else y
			
data Bool232 = True232 | False232
and232 :: Bool232 -> Bool232 -> Bool232
and232 b1 False232 = False232
and232 False232 b2 = False232
and232 b1 b2       = True232

max3 x y z = let
             u = maxim x y
             in (maxim  u z)
             
f1 :: Integer -> Integer -> Integer
f1 x y = x * x + y * y

paritate :: Integer -> String
paritate x = if mod x 2 == 0 then "par" else "impar"

factorial :: Integer ->  Integer
factorial x = if x == 0 then 1 else x * factorial(x - 1)

factorial2 :: Integer ->  Integer
factorial2 0 = 1
factorial2 x = x * factorial2(x - 1)

factorial3 :: Integer ->  Integer
factorial3 x
	|x == 0 = 1
	|otherwise = x * factorial3 (x - 1)
	
max_lista :: [Integer] -> Integer
max_lista [elem] = elem
max_lista (head:tail) = maxim head (max_lista tail)
--max_lista (head:tail) = maxim head $ max_lista tail

poly :: Double -> Double -> Double -> Double -> Double
poly  a b c x = a * x * x + b * x + c

fizzbuzz :: Integer -> String
fizzbuzz  x = if (x `mod` 15 == 0)
			then "FizzBuzz"
		else if(x `mod` 3 == 0)
			then "Fizz"
		else if(x `mod` 5 == 0)
			then "Buzz"
		else " "
		
eeny :: Integer -> String
eeny x = if (x `mod` 2 == 0)
		then "eeny"
	else "meeny"

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n - 1) k + binomial (n - 1) (k - 1)
