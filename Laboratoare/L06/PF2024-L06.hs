-- PF2024-Lab6.hs

-- 1. Fructe
data Fruct = Mar String Bool | Portocala String Int

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = foldr (\fruct acc -> case fruct of
    Portocala soi nrFelii | ePortocalaDeSicilia (Portocala soi nrFelii) -> acc + nrFelii
    _ -> acc) 0

nrMereViermi :: [Fruct] -> Int
nrMereViermi = foldr (\fruct acc -> case fruct of
    Mar _ True -> acc + 1
    _ -> acc) 0

-- 2. Animale
data Animal = Pisica String | Caine String String deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Pisica _) = Nothing

-- 3. Matrice
data Linie = L [Int] deriving Show
data Matrice = M [Linie] deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = all (\(L linie) -> sum linie == n) linii

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = all (\(L linie) -> length linie == n && all (> 0) linie) linii

corect :: Matrice -> Bool
corect (M []) = True
corect (M (L l:ls)) = all (\(L linie) -> length linie == length l) ls
