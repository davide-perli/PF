import Data.Char (isDigit, digitToInt)
--1 a
verifL :: [Int] -> Bool
verifL  l = (length l) `mod` 2 == 0
verifL2  l = even (length l)
verifL3 l = even $ length l

--1 b
takefinal :: [a] -> Int -> [a]
takefinal l n
    | n < length l =    drop (length l - n) l
    | otherwise =   l

remove :: [Int] -> Int -> [Int]
remove l n = (take (n - 1) l) ++ (drop n l)


semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : aux -- concateneaza cu lista care a ramas asupra careia se va aplica din nou functia
 | otherwise = aux
 where aux = semiPareRec t -- creeaza un domeniu local

--2 a
myreplicate :: Int -> Int -> [Int]
myreplicate 0 v = []
myreplicate n v = v:(myreplicate (n-1) v)

--2 b 
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    | even h    = sumImp t
    | otherwise = sumImp t + h

--2 c
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t) =
    if length h > 1 && (h !! 1 == 'A') then 1 + totalLen t 
    else totalLen t

totalLen2 :: [String] -> Int
totalLen2 [] = 0
totalLen2 (('A':string):t) = 1 + length string + totalLen2 t
totalLen2 (_:t) = totalLen2 t

--3
nrVocale :: [String] -> Int
nrVocale = undefined
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9

-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

-- divizori 4 == [1,2,4]
--5
divizori :: Int -> [Int]
divizori n = [d | d <- [1..abs n], n `mod` d == 0]

--listadiv :: [Int] -> [[Int]]
--listadiv = undefined
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]
--6
listadiv :: [Int] -> [[Int]]
listadiv nums = [divizori n | n <- nums]

--7 a 
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec x y [] = []
inIntervalRec x y (h:t) = if ( x <= h && h <=  y) then h:(inIntervalRec x y t)
else inIntervalRec x y t

--7 b 
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp low high nums = [x | x <- nums, x >= low, x <= high]

--8 a 
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
    | x > 0     = 1 + pozitiveRec xs
    | otherwise = pozitiveRec xs

--8 b 
pozitiveComp :: [Int] -> Int
pozitiveComp l = length [n | n <- l, n > 0]

--9 a 
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec xs = pozitiiImpareAux xs 1
  where
    pozitiiImpareAux [] _ = []
    pozitiiImpareAux (x:xs) poz
        | odd x     = poz : pozitiiImpareAux xs (poz + 1)
        | otherwise = pozitiiImpareAux xs (poz + 1)


--9 b 
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [poz | (x, poz) <- zip xs [1..], odd x]

--10 a 
multDigitsRec :: String -> Int
multDigitsRec [] = 1  -- dacă nu avem cifre intoarce 1
multDigitsRec (head:tail)
    | isDigit head = digitToInt head * multDigitsRec tail
    | otherwise = multDigitsRec tail

--10 b 
multDigitsComp :: String -> Int
multDigitsComp s = product [digitToInt x | x <- s, isDigit x] `orElse` 1
  where
    orElse 0 y = y  -- dacă lista e goală -> întorc 1
    orElse x _ = x


-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

-- pozitive [0,1,-3,-2,8,-1,6] == 3

-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1