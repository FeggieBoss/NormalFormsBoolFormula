module FormulaTransformation where

import FormulaDeclaration

{-
 применяем функцию ко всем подформулам от корня к листам
 функция не должна менять атом, чтобы использовать его как базу рекурсии!
 или любой атом - неподвижная точка bar
-}
applyToFormula :: (Formula -> Formula) -> Formula -> Formula 
applyToFormula bar f = let apply = applyToFormula bar in
    case bar f of 
        p@(Atom _)    -> p
        (Not f)       -> Not . apply $ f
        (f1 `And` f2) -> (apply f1) `And` (apply f2)
        (f1 `Or` f2)  -> (apply f1) `Or` (apply f2)
        (f1 :->: f2)  -> (apply f1) :->: (apply f2) 
        (f1 :<->: f2) -> (apply f1) :<->: (apply f2) 

-- применяем функцию ко всем подформулам от листов к корню
applyToFormula' :: (Formula -> Formula) -> Formula -> Formula 
applyToFormula' bar f = let apply = applyToFormula' bar in 
    case f of
        p@(Atom _)    -> bar p 
        (Not f)       -> bar . Not . apply $ f
        (f1 `And` f2) -> bar $ (apply f1) `And` (apply f2)
        (f1 `Or` f2)  -> bar $ (apply f1) `Or` (apply f2)
        (f1 :->: f2)  -> bar $ (apply f1) :->: (apply f2) 
        (f1 :<->: f2) -> bar $ (apply f1) :<->: (apply f2) 

-- убираем -> и <->
normalizator :: Formula -> Formula
normalizator = (applyToFormula' arrowNorm) . (applyToFormula' doubleArrowNorm)
    where -- убираем ->
          arrowNorm :: Formula -> Formula
          arrowNorm (f1 :->: f2)  = (Not f1) `Or` f2
          arrowNorm f = f
          
          -- убираем <->
          doubleArrowNorm :: Formula -> Formula
          doubleArrowNorm (f1 :<->: f2) = (f1 :->: f2) `And` (f2 :->: f1)  
          doubleArrowNorm f = f


-- переводим в NNF
toNNF :: Formula -> Formula
toNNF = (applyToFormula $ simpleToNNF . multipleNotNorm) . normalizator
    where -- упрощаем формулу только с not, or, and, используя законы Де Моргана
          simpleToNNF :: Formula -> Formula
          simpleToNNF (Not f)       = case f of
              p@(Atom _)    -> Not p 
              (Not f)       -> f
              (f1 `And` f2) -> (Not f1) `Or` (Not f2)
              (f1 `Or` f2)  -> (Not f1) `And` (Not f2)
          simpleToNNF f = f

          -- убираем несколько not
          multipleNotNorm :: Formula -> Formula
          multipleNotNorm (Not f) = case f of
              (Not f')  -> multipleNotNorm f'
              otherwise -> Not f
          multipleNotNorm f = f

-- переводим в DNF
toDNF :: Formula -> Formula
toDNF = (applyToFormula' nnfToDNF) . toNNF
    where -- NNF к DNF, используя законы дистрибутивности с and
          nnfToDNF :: Formula -> Formula
          nnfToDNF (f1 `And` f2) = case f2 of
              (f2'1 `Or` f2'2) -> (conjunctionDNFs f1 f2'1) `Or` (conjunctionDNFs f1 f2'2)
              otherwise        -> case f1 of 
                  (f1'1 `Or` f1'2) -> (conjunctionDNFs f1'1 f2) `Or` (conjunctionDNFs f1'2 f2)
                  otherwise        -> f1 `And` f2
          nnfToDNF f = f
          
          -- "and" of two DNF's
          conjunctionDNFs :: Formula -> Formula -> Formula
          conjunctionDNFs (f1 `Or` f2) f3 = (conjunctionDNFs f1 f3) `Or` (conjunctionDNFs f2 f3)
          conjunctionDNFs f1 (f2 `Or` f3) = (conjunctionDNFs f1 f2) `Or` (conjunctionDNFs f1 f3)
          conjunctionDNFs f1 f2 = f1 `And` f2

-- переводим в CNF
toCNF :: Formula -> Formula
toCNF = (applyToFormula' nnfToCNF) . toNNF
    where -- NNF к CNF, используя законы дистрибутивности с or
          nnfToCNF :: Formula -> Formula
          nnfToCNF (f1 `Or` f2) = case f2 of
              (f2'1 `And` f2'2) -> (disjunctionCNFs f1 f2'1) `And` (disjunctionCNFs f1 f2'2)
              otherwise         -> case f1 of 
                  (f1'1 `And` f1'2) -> (disjunctionCNFs f1'1 f2) `And` (disjunctionCNFs f1'2 f2)
                  otherwise         -> f1 `Or` f2
          nnfToCNF f = f
          
          -- "or" of two CNF's
          disjunctionCNFs :: Formula -> Formula -> Formula
          disjunctionCNFs (f1 `And` f2) f3 = (disjunctionCNFs f1 f3) `And` (disjunctionCNFs f2 f3)
          disjunctionCNFs f1 (f2 `And` f3) = (disjunctionCNFs f1 f2) `And` (disjunctionCNFs f1 f3)
          disjunctionCNFs f1 f2 = f1 `Or` f2