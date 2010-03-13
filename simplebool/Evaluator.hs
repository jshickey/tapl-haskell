module Evaluator (evalTerms, parseAndEval, sub, shift) where

import Control.Monad

import TaplError
import Parser
import SimpleBool
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
          walk c (TmAbs name ty t12) = TmAbs name ty $ walk (c + 1) t12
          walk c TmTrue = TmTrue
          walk c TmFalse = TmFalse
          walk c (TmIf t1 t2 t3) = TmIf (walk c t1) (walk c t2) (walk c t3)

-- unlike in TAPL, we ignore the context in this implementation, 
-- since we don't need it for lambda calculus.
eval1 :: Term -> ThrowsError (Maybe Term)
eval1 (TmApp (TmAbs _ _ t12) t2) | isval t2 = return $ Just $ shift (-1) (sub 0 (shift 1 t2) t12)
eval1 (TmApp t1 t2) | isval t1  = liftM (liftM        (TmApp t1)) (eval1 t2)
                    | otherwise = liftM (liftM ((flip TmApp) t2)) (eval1 t1)
eval1 (TmIf TmTrue t2  _) = return $ Just t2
eval1 (TmIf TmFalse _ t3) = return $ Just t3
eval1 (TmIf t1 t2 t3) = liftM (liftM (\t1' -> TmIf t1' t2 t3)) (eval1 t1)
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
parseAndEval str = do parsed <- parseSimpleBool str
                      evaled <- evalTerms parsed
                      return $ showTerms evaled newContext

{- ----------------------------
   TYPING
 ---------------------------- -}

typeof :: Term -> Context -> Ty
typeof TmTrue _ = TyBool
typeof TmFalse _ = TyBool
typeof (TmIf p c a) ctx
    | typeof p ctx /= TyBool = error "predicate must be a bool"
    | otherwise = let tyC = typeof c ctx
                      tyA = typeof a ctx
                  in if tyC == tyA
                     then tyC
                     else error "conditional and alternative of an if-statement must be of the same type"
typeof v@(TmVar _ _) ctx 
    = let (_,(VarBind ty)) = getBindingByIndex ctx (index v)
      in ty

typeof (TmAbs name tyVar body) ctx 
       = let ctx' = appendBinding ctx name $ VarBind tyVar
             tyBody = typeof body ctx'
         in TyArr tyVar tyBody
typeof (TmApp t1 t2) ctx 
    = let tyT1 = typeof t1 ctx
          tyT2 = typeof t2 ctx
      in case tyT1 of
           (TyArr tyArr1 tyArr2) -> if tyT2 == tyArr1
                                     then tyArr2
                                     else error "type mismatch in TmApp"
           otherwise -> error "TmApp must be of an array type"

{- ----------------------------
   PRINTING
 ---------------------------- -}

showTerms :: [Term] -> Context -> String
showTerms [] _ = ""
-- handle TmBind here, because it is the only that affects
-- terms later in the sequence (as opposed to an abstraction,
-- which affect terms within its subtree)
showTerms ((TmBind name b@(VarBind ty)):ts) ctx 
    = name ++ " : " ++ show ty ++ "\n" ++
      showTerms ts (appendBinding ctx name b)
showTerms (t:ts) ctx = showInCtx t ctx ++ " : " ++ show (typeof t ctx) ++ 
                       "\n" ++ showTerms ts ctx
