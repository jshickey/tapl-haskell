module Parser ( parseFullSimple ) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Error

import FullSimple
import Context
import TaplError

parseTerms = return [TmTrue]

parseFullSimple :: String -> ThrowsError [Term]
parseFullSimple str 
    = case runParser parseTerms newContext "fullsimple Parser" str of
        Left err -> throwError $ ParserError $ show err
        Right ts -> return ts