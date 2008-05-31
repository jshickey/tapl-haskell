{- Defines the terms, types, and bindings used in the rcdsubbot
 -}

module Syntax where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import TaplError    

{- --------------------------------
   TERMS
   -------------------------------- -}

data Term = TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmRecord [(String, Term)]
          | TmProj Term String
          deriving (Show, Eq)

isval :: Term -> Bool
isval (TmAbs _ _ _) = True
isval (TmRecord fs) = and $ map (\(_,t) -> isval t) fs
isval t             = False

{- --------------------------------
   TYPES
   -------------------------------- -}

data Ty = TyArr Ty Ty -- "t1 -> t2"
        | TyRecord [(String, Ty)]
        | TyTop
        | TyBot
          deriving (Show, Eq)

{- --------------------------------
   BINDING
   -------------------------------- -}

data Binding = VarBind Ty
               deriving (Show, Eq)
