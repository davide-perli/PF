import Prelude (Int, Maybe(..), Show(..), Eq(..), Ord(..), Floating, pi, (+), (*), (++), concatMap, (<), (>), otherwise, uncurry, map, filter, foldr, fst, snd, lookup, (.))


-- Ex1
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  lookupC :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys = map fst . toList
  values :: c key value -> [value]
  values = map snd . toList
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key, value)] -> c key value
  fromList = foldr (uncurry insert) empty


-- Ex2
newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty = PairList []
  singleton k v = PairList [(k, v)]
  insert k v (PairList xs) = PairList ((k, v) : filter (\(key, _) -> key /= k) xs)
  lookupC k (PairList xs) = lookup k xs
  delete k (PairList xs) = PairList (filter (\(key, _) -> key /= k) xs)
  keys (PairList xs) = map fst xs
  values (PairList xs) = map snd xs
  toList (PairList xs) = xs
  fromList = PairList


-- Ex3
data SearchTree key value
  = Empty
  | BNode (SearchTree key value) key (Maybe value) (SearchTree key value)
  deriving (Show)

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  insert k v Empty = BNode Empty k (Just v) Empty
  insert k v (BNode left key val right)
    | k < key   = BNode (insert k v left) key val right
    | k > key   = BNode left key val (insert k v right)
    | otherwise = BNode left key (Just v) right
  lookupC _ Empty = Nothing
  lookupC k (BNode left key val right)
    | k < key   = lookupC k left
    | k > key   = lookupC k right
    | otherwise = val

  delete k Empty = Empty
  delete k (BNode left key val right)
    | k < key   = BNode (delete k left) key val right
    | k > key   = BNode left key val (delete k right)
    | otherwise = BNode left key Nothing right

  keys Empty = []
  keys (BNode left key _ right) = keys left ++ [key] ++ keys right

  values Empty = []
  values (BNode left _ Nothing right) = values left ++ values right
  values (BNode left _ (Just v) right) = values left ++ [v] ++ values right

  toList Empty = []
  toList (BNode left key Nothing right) =
    toList left ++ toList right
  toList (BNode left key (Just v) right) =
    toList left ++ [(key, v)] ++ toList right

  fromList = foldr (uncurry insert) empty


-- Ex4
data Punct = Pt [Int]

instance Show Punct where
  show (Pt xs) = showTuple xs where
    showTuple []     = "()"
    showTuple ys     = "(" ++ concatMap show ys ++ ")"


-- Ex5
data Arb = Vid | F Int | N Arb Arb deriving Show

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

instance ToFromArb Punct where
  toArb (Pt [])     = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
  
  fromArb Vid             = Pt []
  fromArb (F x)           = Pt [x]
  
  fromArb (N left right) =
    let Pt leftPts  = fromArb left 
        Pt rightPts = fromArb right 
     in Pt (leftPts ++ rightPts)


-- Ex6
data Geo a = Square a | Rectangle a a | Circle a deriving Show

class GeoOps g where
  perimeter :: Floating a => g a -> a
  area :: Floating a => g a -> a

instance GeoOps Geo where
  perimeter (Square a)       = 4 * a
  perimeter (Rectangle a b)   = 2 * (a + b)
  perimeter (Circle r)       = 2 * pi * r
  
  area (Square a)            = a * a 
  area (Rectangle a b)      = a * b 
  area (Circle r)           = pi * r * r 


-- Ex7
instance (Floating a, Eq a) => Eq (Geo a) where
  g1 == g2 = perimeter g1 == perimeter g2
