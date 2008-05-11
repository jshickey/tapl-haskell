module Arith where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
            deriving (Eq)

instance Show Term where
    show TmTrue = "true"
    show TmFalse = "false"
    show (TmIf p c a) = "if " ++ show p ++ " then " ++ show c ++ 
                        " else " ++ show a
    show TmZero = "0"
    show (TmSucc t) | isnumerical t = showAsNum t 1 
                    | otherwise = "(succ " ++ show t ++ ")"
                    where showAsNum TmZero num = show num
                          showAsNum (TmSucc t) num = showAsNum t (num + 1)
    show (TmPred t) = "(pred " ++ show t ++ ")"
    show (TmIsZero t) = "iszero " ++ show t

showTerms :: [Term] -> String
showTerms [] = ""
showTerms (t:ts) = show t ++ "\n" ++ showTerms ts

isnumerical :: Term -> Bool
isnumerical TmZero = True
isnumerical (TmSucc t) = isnumerical t
isnumerical _ = False

