{-# LANGUAGE InstanceSigs #-}
module Main where


import System.Environment (getArgs)
import FormulaDeclaration
import FormulaInstances
import FormulaTransformation

main :: IO ()
main = do
    input <- getArgs
    let f = read $ input!!0
    {- 
      TODO
      exceptions
    -}
    putStrLn $ concat ["Your formula - ", show f]
    putStrLn $ concat ["to NNF - ", show . toNNF $ f]
    putStrLn $ concat ["to DNF - ", show . toDNF $ f]
    putStrLn $ concat ["to CNF - ", show . toCNF $ f]
    