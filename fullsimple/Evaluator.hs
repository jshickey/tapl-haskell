module Evaluator where

import Control.Monad

import Context
import FullSimple
import Parser
import TaplError

-- todo: ContextThrowsError
-- eval :: Term -> Context -> Throws

evalTerms :: [Term] -> ThrowsError [Term]
evalTerms ts = return ts

parseAndEval :: String -> ThrowsError String
parseAndEval str = parseFullSimple str >>= evalTerms >>= showTerms