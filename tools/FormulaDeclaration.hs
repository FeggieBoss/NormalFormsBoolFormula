{-# LANGUAGE InstanceSigs #-}
module FormulaDeclaration where

data Formula = Atom String
             | Not Formula
             | Formula `And` Formula
             | Formula `Or` Formula
             | Formula :->: Formula
             | Formula :<->: Formula
             deriving Eq

instance Show Formula where
    showsPrec :: Int -> Formula -> ShowS
    showsPrec _ (Atom p)      = showString p 
    showsPrec n (Not f)       = showParen (n > 5) (showString "!" . showsPrec 6 f)
    showsPrec n (f1 `And` f2) = showParen (n > 4) (showsPrec 5 f1 . showString " & " . showsPrec 5 f2)
    showsPrec n (f1 `Or` f2)  = showParen (n > 3) (showsPrec 4 f1 . showString " | " . showsPrec 4 f2)
    showsPrec n (f1 :->: f2)  = showParen (n > 2) (showsPrec 3 f1 . showString " -> " . showsPrec 3 f2)
    showsPrec n (f1 :<->: f2) = showParen (n > 1) (showsPrec 2 f1 . showString " <-> " . showsPrec 2 f2)