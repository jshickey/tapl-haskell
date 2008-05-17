module Context where

import FullSimple
import TaplError

newtype Context = Ctx [String]
    deriving (Show)

newContext :: Context
newContext = Ctx []

showTerms :: [Term] -> ThrowsError String
showTerms ts = return "true : Bool"