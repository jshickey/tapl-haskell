module Evaluator (eval, evalTerms, parseAndEval) where

import Control.Monad.Error

import Arith
import ArithParser
import TaplError

-- Use the Maybe monad so that we can signify
-- that no rule applies by returning "Nothing".
eval1 :: Term -> ThrowsError (Maybe Term)
eval1 (TmIf TmTrue conseq _) = return $ Just conseq
eval1 (TmIf TmFalse _ alt)   = return $ Just alt
eval1 (TmIf pred conseq alt) = recurseAndConstruct (\x -> TmIf x conseq alt) pred
eval1 (TmSucc t)             = recurseAndConstruct TmSucc t
eval1 (TmPred TmZero)        = return $ Just TmZero
eval1 (TmPred (TmSucc nv)) 
    | isnumerical nv         = return $ Just nv
eval1 (TmPred t)             = recurseAndConstruct TmPred t
eval1 (TmIsZero TmZero)      = return $ Just TmTrue
eval1 (TmIsZero (TmSucc nv))
    | isnumerical nv         = return $ Just TmFalse
eval1 (TmIsZero t)           = recurseAndConstruct TmIsZero t
eval1 _                      = return Nothing
    
-- Two liftMs, to get inside both the ThrowsError and Maybe monads
recurseAndConstruct :: (Term -> Term) -> Term -> ThrowsError (Maybe Term)
recurseAndConstruct f t = liftM (liftM f) (eval1 t)

eval :: Term -> ThrowsError Term
eval t = do mt' <- eval1 t
            case mt' of
              Nothing -> return t
              Just t' -> eval t'

evalTerms :: [Term] -> ThrowsError [Term]
evalTerms [] = return []
evalTerms (t:ts) = do t' <- eval t
                      ts' <- evalTerms ts
                      return (t':ts')

parseAndEval :: String -> ThrowsError String
parseAndEval str = do parsed <- parseArith str
                      evaled <- evalTerms parsed
                      return $ showTerms evaled