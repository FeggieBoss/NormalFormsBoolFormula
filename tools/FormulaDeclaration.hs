module FormulaDeclaration where

data Formula = Atom String
             | Not Formula
             | Formula `And` Formula
             | Formula `Or` Formula
             | Formula :->: Formula
             | Formula :<->: Formula