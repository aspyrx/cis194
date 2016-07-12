{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import qualified ExprT as ET
import Parser
import StackVM

eval :: ET.ExprT -> Integer
eval (ET.Lit n) = n
eval (ET.Add x y) = (eval x) + (eval y)
eval (ET.Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = case parseExp ET.Lit ET.Add ET.Mul s of
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
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

instance Expr Program where
    lit n = [PushI n]
    add x y = x ++ y ++ [Mul]
    mul x y = x ++ y ++ [Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul


