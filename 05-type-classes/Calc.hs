{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import qualified ExprT as ET
import qualified Parser
import qualified StackVM
import qualified Data.Map as M

eval :: ET.ExprT -> Integer
eval (ET.Lit n) = n
eval (ET.Add x y) = (eval x) + (eval y)
eval (ET.Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case Parser.parseExp ET.Lit ET.Add ET.Mul s of
            Nothing -> Nothing
            Just e -> Just $ eval e

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ET.ExprT where
    lit = ET.Lit
    add = ET.Add
    mul = ET.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = lit $ max a b
    mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = lit $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = lit $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = Parser.parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
    lit n = [StackVM.PushI n]
    add x y = x ++ y ++ [StackVM.Mul]
    mul x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe StackVM.Program
compile = Parser.parseExp lit add mul


class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

bothJust :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
bothJust Nothing _ _ = Nothing
bothJust _ Nothing _ = Nothing
bothJust (Just a) (Just b) f = Just $ f a b

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add x y = \m -> bothJust (x m) (y m) (+)
    mul x y = \m -> bothJust (x m) (y m) (*)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs

testWithVars :: Maybe Integer
testWithVars = withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))

