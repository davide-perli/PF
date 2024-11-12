-- PF2024-Lab5.hs

-- 1. Calculați suma pătratelor elementelor impare dintr-o listă dată ca parametru
sumOfSquaresOfOdds :: [Int] -> Int
sumOfSquaresOfOdds = foldr (\x acc -> if odd x then x^2 + acc else acc) 0

-- 2. Scrieți o funcție care verifică că toate elementele dintr-o listă sunt True, folosind foldr
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- 3. Scrieți o funcție care verifică dacă toate elementele dintr-o listă de numere întregi satisfac o proprietate dată ca parametru
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p = foldr (\x acc -> p x && acc) True

-- 4. Scrieți o funcție care verifică dacă există elemente într-o listă de numere întregi care satisfac o proprietate dată ca parametru
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p = foldr (\x acc -> p x || acc) False

-- 5. Redefiniți funcțiile map și filter folosind foldr. Le puteți numi mapFoldr și filterFoldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x : acc else acc) []

-- 6. Folosind funcția foldl, definiți funcția listToInt care transformă o listă de cifre într-un număr întreg
listToInt :: [Integer] -> Integer
listToInt = foldl (\acc x -> acc * 10 + x) 0

-- 7a. Scrieți o funcție care elimină toate aparițiile unui caracter dat dintr-un șir de caractere
rmChar :: Char -> String -> String
rmChar c = filter (/= c)

-- 7b. Scrieți o funcție recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (c:cs) str = rmCharsRec cs (rmChar c str)

-- 7c. Scrieți o funcție echivalentă cu cea de la (b) care folosește rmChar și foldr
rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr rmChar str chars

-- 8. Scrieți o funcție myReverse care primește ca parametru o listă de întregi și întoarce lista elementelor în ordine inversă
myReverse :: [Int] -> [Int]
myReverse = foldl (flip (:)) []

-- 9. Scrieți un predicat myElem care verifică apartenența unui întreg la o listă de întregi
myElem :: Int -> [Int] -> Bool
myElem e = foldr (\x acc -> x == e || acc) False

-- 10. Scrieți o funcție myUnzip care transformă o listă de perechi într-o pereche de liste
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], [])

-- 11. Scrieți o funcție union care întoarce lista reuniunii a două liste de întregi primite ca parametri
union :: [Int] -> [Int] -> [Int]
union xs ys = xs ++ filter (`notElem` xs) ys

-- 12. Scrieți o funcție intersect care întoarce lista intersecției a două liste de întregi primite ca parametri
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = filter (`elem` ys) xs

-- 13. Scrieți o funcție permutations care întoarce lista tuturor permutărilor elementelor unei liste de întregi primite ca parametru
permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [y:zs | y <- xs, zs <- permutations (filter (/= y) xs)]
