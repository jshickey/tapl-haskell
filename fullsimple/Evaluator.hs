module Evaluator ( parseAndEval ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Syntax
import Parser
import TaplError

eval1 :: Term -> ContextThrowsError (Maybe Term)
eval1 t@(TmBind var binding) 
      = case binding of
          VarBind ty -> do ctx <- get
                           put $ appendBinding var binding ctx
                           return Nothing
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