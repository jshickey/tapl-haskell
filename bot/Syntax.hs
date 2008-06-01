{- Defines the terms, types, and bindings used in the bot
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
          deriving (Show, Eq)

isval :: Term -> Bool
isval (TmAbs _ _ _) = True
isval t             = False

{- --------------------------------
   TYPES
   -------------------------------- -}

data Ty = TyArr Ty Ty -- "t1 -> t2"
        | TyTop
        | TyBot
          deriving (Show, Eq)

{- --------------------------------
   BINDING
   -------------------------------- -}

data Binding = VarBind Ty
               deriving (Show, Eq)
