-- Definirea tipului de date pentru arbori ternari
data Tree = Empty 
          | Node Int Tree Tree Tree 
          deriving (Show)

-- Clasa ArbInfo
class ArbInfo t where
    level :: t -> Int
    sumval :: t -> Int
    nrFrunze :: t -> Int

-- Instanța pentru tipul Tree
instance ArbInfo Tree where
    level Empty = 0
    level (Node _ left middle right) = 1 + maximum [level left, level middle, level right]

    sumval Empty = 0
    sumval (Node value left middle right) = value + sumval left + sumval middle + sumval right

    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ left middle right) = nrFrunze left + nrFrunze middle + nrFrunze right








-- Clasa Scalar
class Scalar a where
    zero :: a
    one :: a
    adds :: a -> a -> a
    mult :: a -> a -> a
    negates :: a -> a
    recips :: a -> a

-- Instanța pentru tipul Integer
instance Scalar Integer where
    zero = 0
    one = 1
    adds = (+)
    mult = (*)
    negates = negate
    recips x = if x /= 0 then 1 `div` x else error "Division by zero"

-- Definirea unui tip de date pentru vectori bidimensionali
data Vector2D s = Vector2D s s deriving (Show)

-- Clasa Vector
class Scalar s => Vector v s where
    zerov :: v s
    onev :: v s
    addv :: v s -> v s -> v s
    smult :: s -> v s -> v s
    negatev :: v s -> v s

-- Instanța pentru Vector2D
instance Scalar s => Vector Vector2D s where
    zerov = Vector2D zero zero
    onev = Vector2D one one

    addv (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (adds x1 x2) (adds y1 y2)
    smult scalar (Vector2D x y) = Vector2D (mult scalar x) (mult scalar y)
    negatev (Vector2D x y) = Vector2D (negates x) (negates y)
