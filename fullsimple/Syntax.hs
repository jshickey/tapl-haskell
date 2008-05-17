module Syntax where

import Control.Monad
import TaplError    

{- --------------------------------
   TERMS
   -------------------------------- -}

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmCase Term [(String, (String, Term))]
          | TmTag String Term Ty
          | TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmLet String Term Term
          | TmFix Term
          | TmString String
          | TmUnit
          | TmAscribe Term Ty
          | TmRecord [(String, Term)]
          | TmProj Term String
          | TmFloat Double
          | TmTimesfloat Term Term
          | TmZero
          | TmSucc Term
          | TmPred
          | TmIsZero Term
          | TmInert Ty
          | TmBind String Binding
          deriving (Show, Eq)

{- --------------------------------
   TYPES
   -------------------------------- -}

data Ty = TyVar Int Int
        | TyId String
        | TyArr Ty Ty -- "t1 -> t2"
        | TyRecord [(String, Ty)]
        | TyVariant [(String, Ty)]
        | TyBool
        | TyString
        | TyFloat
        | TyNat
          deriving (Show, Eq)


{- --------------------------------
   CONTEXT & BINDING
   -------------------------------- -}

data Binding = NameBind
             | TyVarBind
             | VarBind Ty
             | TmAbbBind Term (Maybe Ty)
             | TyAbbBind Ty
               deriving (Show, Eq)

newtype Context = Ctx [(String, Binding)]
    deriving (Show, Eq)

newContext :: Context
newContext = Ctx []

{- --------------------------------
   PRINTING
   -------------------------------- -}



showTerms :: [Term] -> ThrowsError String
showTerms ts = return "true : Bool"

