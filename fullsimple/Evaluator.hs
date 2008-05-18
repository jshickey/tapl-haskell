module Evaluator ( parseAndEval ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Syntax
import Parser
import TaplError

sub i val t = walk 0 t subVar
    where subVar c v@(TmVar idx _) | c + i == idx = shift c val
                                   | otherwise    = v

shift i t = walk 0 t shiftVar
    where shiftVar c (TmVar idx ctxLen) 
              | idx >= c  = TmVar (idx + i) (ctxLen + i)
              | otherwise = TmVar idx (ctxLen + i)

walk c t f = case t of
               TmVar _ _ -> f c t
               TmAbs var ty body -> TmAbs var ty $ walk (c + 1) body f
               TmApp t1 t2 -> TmApp (walk c t1 f) (walk c t2 f)
               otherwise -> t

eval1Cons :: (Term -> Term) -> Term -> ContextThrowsError (Maybe Term)
eval1Cons constructor = (liftM (liftM constructor)) . eval1

apply term body = shift (-1) $ sub 0 (shift 1 term) body

eval1 :: Term -> ContextThrowsError (Maybe Term)
eval1 t@(TmBind var binding) 
      = case binding of
          VarBind ty -> do ctx <- get
                           put $ appendBinding var binding ctx
                           return Nothing
eval1 (TmVar idx ctxLen) = do ctx <- get
                              binding <- liftThrows $ bindingOf idx ctx 
                              case binding of
                                TmAbbBind val _ -> return $ Just val
                                otherwise       -> return Nothing
eval1 (TmApp t1@(TmAbs _ _ body) t2) 
    | isval t2  = return $ Just $ apply t2 body
    | otherwise = eval1Cons (TmApp t1) t2
eval1 (TmApp t1 t2) | not $ isval t1 = eval1Cons ((flip TmApp) t2) t1
eval1 _ = return Nothing

eval :: Term -> ContextThrowsError Term
eval t = do mt' <- eval1 t
            case mt' of
              Nothing -> return t
              Just t' -> eval t'           

evalTerms :: [Term] -> ThrowsError [Term]
evalTerms ts = runContextThrows $ mapM eval ts

parseAndEval :: String -> ThrowsError String
parseAndEval str = parseFullSimple str >>= evalTerms >>= showTerms