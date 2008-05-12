module Typing where

import Control.Monad.Error
import Control.Monad
import qualified Arith as A -- we will use our own showTerms
import ArithParser
import TaplError
import qualified Evaluator as E

data Type = Bool
          | Nat
            deriving (Show, Eq)

typeof :: A.Term -> ThrowsError Type
typeof A.TmTrue = return Bool
typeof A.TmFalse = return Bool
typeof (A.TmIf t1 t2 t3) = do tyT1 <- typeof t1
                              case tyT1 of
                                Bool -> do tyT2 <- typeof t2
                                           tyT3 <- typeof t3
                                           if tyT2 == tyT3
                                             then return tyT2
                                             else throwError condAltMismatch
                                _ -> throwError predMismatch
typeof A.TmZero = return Nat
typeof (A.TmSucc t) = verifyNat t Nat
typeof (A.TmPred t) = verifyNat t Nat 
typeof (A.TmIsZero t) = verifyNat t Bool

predMismatch = TypeMismatch "The predicate of an if statement must be of type Bool"
condAltMismatch = TypeMismatch "The conditional and alternative of an if statement must be of the same type"
expectedNat = TypeMismatch "Expected a Nat"

verifyNat :: A.Term -> Type -> ThrowsError Type
verifyNat term ty = do tyTerm <- typeof term
                       if tyTerm == Nat
                         then return ty
                         else throwError expectedNat

showTerms :: [A.Term] -> ThrowsError String
showTerms [] = return ""
showTerms (t:ts) = do tyT <- typeof t
                      rest <- showTerms ts
                      return $ show t ++ " : " ++ show tyT ++ "\n" ++ rest

parseAndEval :: String -> ThrowsError String
parseAndEval str = do parsed <- parseArith str
                      evaled <- E.evalTerms parsed
                      showTerms evaled