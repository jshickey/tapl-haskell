module Evaluator (evalTerms, parseAndEval, sub, shift) where

import Control.Monad

import TaplError
import UntypedParser
import Untyped
import Context

sub :: Int -> Term -> Term -> Term
sub subIdx replacement term = helper term varSub
    where varSub c v@(TmVar _ _) 
              | index v == c + subIdx = shift c replacement
              | otherwise             = v

shift :: Int -> Term -> Term
shift inc term = helper term varShift
    where varShift c (TmVar idx len)
              | idx >= c  = TmVar (idx + inc) (len + inc)
              | otherwise = TmVar idx         (len + inc)

-- contains common functionality of "sub" and "shift"
helper :: Term -> (Int -> Term -> Term) -> Term
helper term varHandler = walk 0 term
    where walk c v@(TmVar _ _)    = varHandler c v
          walk c (TmApp t1 t2)    = TmApp (walk c t1) (walk c t2)
          walk c (TmAbs name t12) = TmAbs name $ walk (c + 1) t12

-- unlike in TAPL, we ignore the context in this implementation, 
-- since we don't need it for lambda calculus.
eval1 :: Term -> ThrowsError (Maybe Term)
eval1 (TmApp (TmAbs _ t12) t2) | isval t2 = return $ Just $ shift (-1) (sub 0 (shift 1 t2) t12)
eval1 (TmApp t1 t2) | isval t1  = liftM (liftM        (TmApp t1)) (eval1 t2)
                    | otherwise = liftM (liftM ((flip TmApp) t2)) (eval1 t1)
eval1 _ = return $ Nothing

-- Small-step evaluation of a term
eval :: Term -> ThrowsError Term
eval t = do mt' <- eval1 t
            case mt' of
              Just t' -> eval t'
              Nothing -> return t

-- Evaluates a sequence of terms, where earlier terms in
-- the sequence (such as binders) affect the context of later terms.
evalTerms :: [Term] -> ThrowsError [Term]
evalTerms [] = return []
evalTerms (t:ts) = liftM2 (:) (eval t) (evalTerms ts)

parseAndEval :: String -> ThrowsError String
parseAndEval str = do parsed <- parseUntyped str
                      evaled <- evalTerms parsed
                      -- Use a new context here.  Both the top-level
                      -- bindings and the bindings within abstractions
                      -- a re-created in the same order.
                      return $ showTerms evaled newContext
