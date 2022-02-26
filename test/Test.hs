{-# LANGUAGE InstanceSigs #-}

import           FormulaDeclaration
import           FormulaTransformation
import           FormulaInstances
import           FormulaSamples
import           Test.QuickCheck
import           Test.Tasty (defaultMain, testGroup, TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)
import qualified Data.ByteString.Char8 as String
import           Data.Char

-- QuickCheck
instance Arbitrary Formula where
    arbitrary :: Gen Formula
    arbitrary = sized arbitraryWithSize
        where arbitraryWithSize :: Int -> Gen Formula
              arbitraryWithSize 0 = pure $ Atom "p0"
              arbitraryWithSize n = 
                  oneof [ Atom <$> arbitraryString
                         , Not <$> resize (n-1) arbitrary
                         , And <$> (resize (n `div` 2) arbitrary) <*> (resize (n `div` 2) arbitrary)
                         , Or <$> (resize (n `div` 2) arbitrary) <*> (resize (n `div` 2) arbitrary)
                         , (:->:) <$> (resize (n `div` 2) arbitrary) <*> (resize (n `div` 2) arbitrary)
                         , (:<->:) <$> (resize (n `div` 2) arbitrary) <*> (resize (n `div` 2) arbitrary)
                        ]
              
              arbitraryString :: Gen String
              arbitraryString = resize 3 $ (\n -> ['p',n]) <$> chr <$> chooseInt (48, 57)

-- проверяем находится ли формула в NNF
isNNF :: Formula -> Bool
isNNF (Atom _)      = True
isNNF (Not f)       = case f of 
                      (Atom _)  -> True 
                      otherwise -> False
isNNF (f1 `And` f2) = isNNF f1 && isNNF f2
isNNF (f1 `Or` f2)  = isNNF f1 && isNNF f2
isNNF _ = False

-- проверяем является ли формула корректным клозом в CNF
isDisjunctionClause :: Formula -> Bool 
isDisjunctionClause (Atom _) = True 
isDisjunctionClause (Not f)  = case f of 
                               (Atom _)  -> True 
                               otherwise -> False
isDisjunctionClause (f1 `Or` f2) = isDisjunctionClause f1 && isDisjunctionClause f2 
isDisjunctionClause _ = False

-- проверяем является ли формула корректным клозом в DNF
isConjunctionClause :: Formula -> Bool 
isConjunctionClause (Atom _)      = True 
isConjunctionClause (Not f)       = case f of 
                                    (Atom _)  -> True 
                                    otherwise -> False
isConjunctionClause (f1 `And` f2) = isConjunctionClause f1 && isConjunctionClause f2 
isConjunctionClause _             = False

-- проверяем находится ли формула в DNF
isDNF :: Formula -> Bool
isDNF (f1 `Or` f2) = isDNF f1 && isDNF f2 
isDNF f = isConjunctionClause f

-- проверяем находится ли формула в CNF
isCNF :: Formula -> Bool 
isCNF (f1 `And` f2) = isCNF f1 && isCNF f2
isCNF f = isDisjunctionClause f

-- проверяет правильно ли осуществлён перевод в нормальную форму
correctNormalForm :: (Formula -> Formula) -> (Formula -> Bool) -> Formula -> Bool 
correctNormalForm toNF isNF f = (\f' -> isNF f' && f'==f) $ toNF f


unitTests :: TestTree
unitTests = testGroup "UnitTests:" [toNNFtests, toDNFtests, toCNFtests]

toNNFtests :: TestTree 
toNNFtests = testGroup
    "NNF Tests"
    [
        testCase "already in NNF" $
            do
               assertEqual "p1 & p2 => ?"                 and1      (toNNF and1)
               assertEqual "p1 | (p2 & (!p1 & !p2)) => ?" mixed6    (toNNF mixed6)
               assertEqual "p1 | (p3 & p4) => ?"          orAnd2    (toNNF orAnd2)
        ,

        testCase "arrow destruction" $
            do
               assertEqual "p1 -> p4 => ?"                orNot1    (toNNF arrow1)
               assertEqual "p1 <-> p3 => ?"               andOrNot1 (toNNF darrow1)
               assertEqual "(p1 & p2) -> (p3 & p4) => ?"  andOrNot2 (toNNF arrowAndAnd)
        ,              

        testCase "lows" $
            do
               assertEqual "!(p1 & p2) => ?"              orNot2    (toNNF notAnd1)
               assertEqual "!(p2 | p4) => ?"              orAnd     (toNNF notOr1)
               assertEqual "!!p1 => ?"                    atom1     (toNNF notnot1)
        ,

        testCase "special cases" $
            do
               assertEqual "!!!(p1 <-> p3) => ?"          mixed7    (toNNF mixed1)
               assertEqual "!(!p1 & !p2) => ?"            or1       (toNNF mixed2)
               assertEqual "p1 | (p2 & !(p1 | p2)) => ?"  mixed6    (toNNF mixed5)
    ]


toDNFtests :: TestTree 
toDNFtests = testGroup
    "DNF Tests"
    [
        testCase "already in DNF" $
            do
               assertEqual "p1 => ?"                                         atom1     (toDNF atom1)
               assertEqual "p1 & ((p1 & p2) & p4) => ?"                      and3      (toDNF and3)
               assertEqual "((p1 | p2) | p4) | ((p1 | p2) | (p2 | p4)) => ?" or3       (toDNF or3)
               assertEqual "(p1 & p2) | p2 => ?"                             orAnd1    (toDNF orAnd1)
               assertEqual "!p4 => ?"                                        not4      (toDNF not4)
               assertEqual "(!p1 | !p2) | (p3 & p4) => ?"                    andOrNot2 (toDNF andOrNot2) 
        ,           

        testCase "lows" $
            do
               assertEqual "p1 & (p1 | p2) => ?"                             orAnd3    (toDNF andOr1)
               assertEqual "(p2 | p4) & p3 => ?"                             orAnd4    (toDNF andOr2)
        ,

        testCase "special cases" $
            do
               assertEqual "(!p1 | p3) & (!p3 | p1) => ?"                    mixed8    (toDNF andOrNot1)
               assertEqual "!((p1 -> p4) & (p1 <-> p3)) => ?"                mixed9    (toDNF mixed3)
               assertEqual "!((p1 & (p1 | p2)) & !(p1 & (p1 | p2))) => ?"    mixed10   (toDNF mixed4)
    ]

toCNFtests :: TestTree 
toCNFtests = testGroup
    "CNF Tests"
    [
        testCase "already in CNF" $
            do
               assertEqual "p1 => ?"                                         atom1     (toCNF atom1)
               assertEqual "p1 & ((p1 & p2) & p4) => ?"                      and3      (toCNF and3)
               assertEqual "((p1 | p2) | p4) | ((p1 | p2) | (p2 | p4)) => ?" or3       (toCNF or3)
               assertEqual "p1 & (p1 | p2) => ?"                             andOr1    (toCNF andOr1)
               assertEqual "!p4 => ?"                                        not4      (toCNF not4)
               assertEqual "(!p1 | p3) & (!p3 | p1) => ?"                    andOrNot1 (toCNF andOrNot1) 
        ,           

        testCase "lows" $
            do
               assertEqual "p1 & (p1 | p2) => ?"                             andOr3    (toCNF orAnd1)
               assertEqual "(p2 | p4) & p3 => ?"                             andOr4    (toCNF orAnd2)
        ,

        testCase "special cases" $
            do
               assertEqual "(p1 & p2) <-> (p3 & p4) => ?"                    mixed11   (toCNF darrowAndAnd)
               assertEqual "(p1 & p2) | (!p2 -> p2) => ?"                    orAnd5    (toCNF orArrowAnd)
               assertEqual "p1 | (p2 & (!p1 & !p2)) => ?"                    mixed12   (toCNF mixed6)
    ]


main :: IO ()
main = do
    putStrLn $ "QuickCheck:"
    mapM_ (quickCheck . (mapSize $ const 10)) [correctNormalForm toNNF isNNF, correctNormalForm toDNF isDNF, correctNormalForm toCNF isCNF]
    defaultMain unitTests