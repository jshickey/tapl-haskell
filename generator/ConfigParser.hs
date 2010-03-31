{-

-}
module ConfigParser (parseConfig) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Char

import Config
import TaplError
    
{- ------------------------------
   Lexer, making use of the Parsec.Token and Language
   modules for ease of lexing programming language constructs
   ------------------------------ -}
configDef = LanguageDef
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = False
                , identStart      = letter 
                , identLetter     = letter <|> digit
                , opStart         = fail "no operators"
                , opLetter        = fail "no operators"
                , reservedOpNames = []
                , caseSensitive   = True
                , reservedNames   = ["files", "terms", "types"
                                    , "tests", "options"]
                }

lexer = P.makeTokenParser configDef

identifier    = P.identifier    lexer
reserved      = P.reserved      lexer
symbol        = P.symbol        lexer
whiteSpace    = P.whiteSpace    lexer
comma         = P.comma         lexer

parseList label = reserved label >>
                  symbol "=" >>
                  identifier `sepBy` comma
                             
copyConfig = liftM CopyConfig (parseList "files")
                             
genConfig = do terms <- parseList "terms"
               types <- parseList "types"
               tests <- parseList "tests"
               options <- parseList "options"
               return $ GenConfig (Terms terms) (Types types)
                          (Tests tests) (Options options)
config = (try copyConfig) <|> genConfig

parseConfig :: String -> ThrowsError Config
parseConfig str =
    case runParser config 0 "config parser" str of
      Left err -> throwError $ ParserError $ show err
      Right c  -> return c
