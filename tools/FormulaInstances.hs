{-# LANGUAGE InstanceSigs #-}
module FormulaInstances where

import FormulaDeclaration
import FormulaCalculation
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- проверка на эквивалентность формул с одинаковым набором переменных
instance Eq Formula where
    (==) :: Formula -> Formula -> Bool 
    f1 == f2 = (findValues f1 vars) == (findValues f2 vars)
        where vars = getArgs f1 -- / getArgs f2
        

-- удобное строковое представление формулы
instance Show Formula where
    showsPrec :: Int -> Formula -> ShowS
    showsPrec _ (Atom p)      = showString p 
    showsPrec n (Not f)       = showParen (n > 5) (showString "!" . showsPrec 6 f)
    showsPrec n (f1 `And` f2) = showParen (n > 4) (showsPrec 5 f1 . showString " & " . showsPrec 5 f2)
    showsPrec n (f1 `Or` f2)  = showParen (n > 3) (showsPrec 4 f1 . showString " | " . showsPrec 4 f2)
    showsPrec n (f1 :->: f2)  = showParen (n > 2) (showsPrec 3 f1 . showString " -> " . showsPrec 3 f2)
    showsPrec n (f1 :<->: f2) = showParen (n > 1) (showsPrec 2 f1 . showString " <-> " . showsPrec 2 f2)

{-
  реализацию была взята из https://wiki.haskell.org/Parsing_expressions_and_statements
  парсим строковое представление формулы
-}
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "!&|-<"
              , opLetter = oneOf "!&-<>"
              , reservedOpNames = ["!", "&", "|", "->", "<->"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Formula
exprparser = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "!" >> return Not)]
        , [Infix (m_reservedOp "&" >> return And) AssocLeft]
        , [Infix (m_reservedOp "|" >> return Or) AssocLeft]
        , [Infix (m_reservedOp "->" >> return (:->:)) AssocLeft]
        , [Infix (m_reservedOp "<->" >> return (:<->:)) AssocLeft]
        ]

term = m_parens exprparser <|> fmap Atom m_identifier

mainparser :: Parser Formula
mainparser = m_whiteSpace >> exprparser <* eof

instance Read Formula where
    readsPrec _ s = case (parse mainparser [] $ s) of
        Right f  -> [(f, [])]