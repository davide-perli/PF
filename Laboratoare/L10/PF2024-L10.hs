import Data.Maybe (fromJust)
import Data.List (nub)

type Nume = String

data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving (Eq)

infixr 2 :|:
infixr 3 :&:
infixr 4 :->:
infixr 4 :<->:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

instance Show Prop where
    show (Var n) = n
    show F = "F"
    show T = "T"
    show (Not p) = "~" ++ show p
    show (p1 :|: p2) = "(" ++ show p1 ++ "|" ++ show p2 ++ ")"
    show (p1 :&: p2) = "(" ++ show p1 ++ "&" ++ show p2 ++ ")"
    show (p1 :->: p2) = "(" ++ show p1 ++ "->" ++ show p2 ++ ")"
    show (p1 :<->: p2) = "(" ++ show p1 ++ "<->" ++ show p2 ++ ")"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var n) env = impureLookup n env
eval F _ = False
eval T _ = True
eval (Not p) env = not (eval p env)
eval (p1 :|: p2) env = eval p1 env || eval p2 env
eval (p1 :&: p2) env = eval p1 env && eval p2 env
eval (p1 :->: p2) env = not (eval p1 env) || eval p2 env
eval (p1 :<->: p2) env = eval p1 env == eval p2 env

variabile :: Prop -> [Nume]
variabile (Var n) = [n]
variabile F = []
variabile T = []
variabile (Not p) = variabile p
variabile (p1 :|: p2) = nub $ variabile p1 ++ variabile p2
variabile (p1 :&: p2) = nub $ variabile p1 ++ variabile p2
variabile (p1 :->: p2) = nub $ variabile p1 ++ variabile p2
variabile (p1 :<->: p2) = nub $ variabile p1 ++ variabile p2

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (n:ns) = map ((n, False):) rest ++ map ((n, True):) rest
    where rest = envs ns

satisfiabila :: Prop -> Bool
satisfiabila prop = any (\env -> eval prop env) allEnvs
    where vars = variabile prop
          allEnvs = envs vars

valida :: Prop -> Bool
valida prop = not $ satisfiabila (Not prop)

echivalenta :: Prop -> Prop -> Bool
echivalenta prop1 prop2 =
    all (\env -> eval prop1 env == eval prop2 env) allEnvs
    where vars = nub $ variabile prop1 ++ variabile prop2
          allEnvs = envs vars

main :: IO ()
main = do 
    print $ eval p1 [("P", True), ("Q", False)]
    print $ satisfiabila (Not (Var "P") :&: Var "Q")
    print $ valida (Not (Var "P") :|: Var "P")
