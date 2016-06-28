{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import StackVM

-- Ex. 1,2

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Ex. 3,4

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = ExprT.Lit x
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)


-- Ex. 5

instance Expr Program where
    lit x = [PushI x]
    add px py = px ++ py ++ [StackVM.Add]
    mul px py = px ++ py ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Ex. 6
