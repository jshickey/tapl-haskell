module TaplError where
{-
 - Adapted from the excellent tutorial:
 - http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/overview.html
-}

import Control.Monad.Error

data TaplError = ParserError String
               | EvalError String
               | Default String
               
instance Show TaplError where
    show (ParserError msg) = "Parse Error: " ++ msg
    show (EvalError msg) = "Evaluation Error: " ++ msg
    show (Default msg) = "Error: " ++ msg

instance Error TaplError where
    noMsg  = Default "An unknown error has occurred"
    strMsg = Default

type ThrowsError = Either TaplError

trapError = (flip catchError) (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runThrows :: ThrowsError String -> String
runThrows action = extractValue $ trapError action

