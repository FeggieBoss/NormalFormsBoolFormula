{-# LANGUAGE InstanceSigs #-}
module FormulaCalculation(
    getArgs
  , calcFormula   
  , getBitMasks 
  , findValues
) where

import Data.List
import FormulaDeclaration

-- находим все переменные формулы
getArgs :: Formula -> [String]
getArgs (Atom p)      = [p]
getArgs (Not f)       = getArgs f 
getArgs (f1 `And` f2) = nub . sort $ getArgs f1 ++ getArgs f2
getArgs (f1 `Or` f2)  = nub . sort $ getArgs f1 ++ getArgs f2
getArgs (f1 :->: f2)  = nub . sort $ getArgs f1 ++ getArgs f2
getArgs (f1 :<->: f2) = nub . sort $ getArgs f1 ++ getArgs f2

-- вычисляем значение формулы
calcFormula :: Formula -> [(String, Bool)] -> Bool
calcFormula (Atom p)      i = findValue p i
    where findValue var i = case (find (\(a,b) -> a == var) i) of
                                Just (_, val) -> val
                                otherwise     -> False
calcFormula (Not f)       i = not $ calcFormula f i
calcFormula (f1 `And` f2) i = and [calcFormula f1 i, calcFormula f2 i]
calcFormula (f1 `Or` f2)  i = or [calcFormula f1 i, calcFormula f2 i]
calcFormula (f1 :->: f2)  i = calcFormula ((Not f1) `Or` f2) i
calcFormula (f1 :<->: f2) i = calcFormula ((f1 :->: f2) `And` (f2 :->: f1)) i 

-- найти все битовые маски определенной длины
getBitMasks :: Int -> [[Bool]]
getBitMasks 1 = [[False],[True]]
getBitMasks n = (map (\l -> False:l) (getBitMasks $ n-1)) ++ (map (\l -> True:l) (getBitMasks $ n-1))

{-
  найти значения формулы при всех значениях конкретных переменных(второй аргумент) 
  значения идут в порядке увеличения значения битовых масок
-}
findValues :: Formula -> [String] -> [Bool]
findValues f vars = map (\i -> calcFormula f i) is
    where is = map (\l -> zip vars l) (getBitMasks $ length vars) 