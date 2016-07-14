{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List
import Data.Tuple
import System.IO

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e : l) $ f + empFun e

instance Monoid GuestList where
    mempty = GL [] 0
    GL x a `mappend` GL y b = GL (x <> y) $ a + b

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f s (Node x []) = f x s
treeFold f s (Node x l) = f x $ map (treeFold f s) l

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = let tupZip f (a, b) (c, d) = (f a c, f b d)
               in swap . foldr (tupZip (<>)) (mempty, e `glCons` mempty)

maxFun :: Tree Employee -> GuestList
maxFun t = let (a, b) = treeFold (nextLevel) [] t
            in max a b

formatList :: GuestList -> String
formatList (GL l f) = "Total fun: " ++ show f ++ '\n' : (unlines . sort . map empName $ l)

main :: IO ()
main = withFile "company.txt" ReadMode (\handle -> do
    file <- hGetContents handle
    (case reads file of
       [] -> return ()
       [(t, _)] -> putStr $ formatList . maxFun $ t))

