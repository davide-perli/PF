factori :: Int -> [Int]
factori val = [i | i <- [1..val], val `mod` i == 0]

prim :: Int -> Bool
prim n = length (factori n) == 2

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = x <= head xs && ordonataNat xs

ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [x] _ = True
ordonata (x:y:xs) f = f x y && ordonata (y:xs) f

firstEl :: [(a, b)] -> [a]
firstEl = map fst

sumList :: [[Int]] -> [Int]
sumList = \lists -> map sum lists

prel2 :: [Int] -> [Int]
prel2 s = [if even x then x `div` 2 else x * 2 | x <- s]

contineCaracter :: Char -> [String] -> [String]
contineCaracter c lista = [s | s <- lista, c `elem` s]

patrate :: [Int] -> [Int]
patrate xs = map (^2) (filter odd xs)

patratepoz :: [Int] -> [Int]
patratepoz l = map ((^2) . fst) (filter (odd . snd) (zip 1 [1..]))