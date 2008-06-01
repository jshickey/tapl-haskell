{- Parsec parser for rcdsub.  The sole expected method, parseRcdsub,
   takes a string as input, and returns a list of terms, where each term
   was separated by a semicolon in the input.
 -}
module Parser ( parseRcdsub ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Char

import Syntax
import Typing
import TaplError
import SimpleContext

{- ------------------------------
   Lexer, making use of the Parsec.Token and Language
   modules for ease of lexing programming language constructs
   ------------------------------ -}
rcdsubDef = LanguageDef
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = ""
                , nestedComments  = False
                , identStart      = letter 
                , identLetter     = letter <|> digit
                , opStart         = fail "no operators"
                , opLetter        = fail "no operators"
                , reservedOpNames = []
                , caseSensitive   = True
                , reservedNames   = ["lambda", "Top", "Bot"]
                }

lexer = P.makeTokenParser rcdsubDef

parens        = P.parens        lexer
braces        = P.braces        lexer
identifier    = P.identifier    lexer
reserved      = P.reserved      lexer
symbol        = P.symbol        lexer
whiteSpace    = P.whiteSpace    lexer
semi          = P.semi          lexer
comma         = P.comma         lexer
colon         = P.colon         lexer
natural       = P.natural       lexer

{- ------------------------------
   Types
   ------------------------------ -}

parseTypeTop = reserved "Top" >> return TyTop

parseTypeBot = reserved "Bot" >> return TyBot

parseTypeRecord = braces $ liftM TyRecord $ sepBy parseField comma
    where parseField = do a <- parseFieldAccessor
                          symbol ":"
                          ty <- parseType
                          return (a,ty)

parseTypeArr = parseTypeTop   <|>
               parseTypeBot    <|>
               parseTypeRecord  <|>
               parens parseType

parseType = parseTypeArr `chainr1` (symbol "->" >> return TyArr)

{- ------------------------------
   Variables
   ------------------------------ -}

parseVar = do var <- identifier
              if (isUpper $ var !! 0)
                then fail "variables must start with a lowercase letter"
                else do ctx <- getState
                        idx <- throwsToParser $ indexOf var ctx
                        return $ TmVar idx (ctxLength ctx)

{- ------------------------------
   lambda
   ------------------------------ -}

parseAbs = do reserved "lambda"
              ctx <- getState
              var <- identifier <|> symbol "_"
              symbol ":"
              ty <- parseType
              symbol "."
              -- temporarily add a binding, while parsing the body,
              -- and then restore the original context at the end
              updateState $ appendBinding var $ VarBind ty
              body <- parseTerm
              setState ctx
              return $ TmAbs var ty body

{- ------------------------------
   Records and Projections
   ------------------------------ -}

-- Fields can either be named or not.  If they are not, then they
-- are numbered starting with 1.  To keep parsing the fields simple,
-- we label them with -1 at first if they have no name.  We then
-- replace the -1's with the correct index as a post-processing step.
parseRecord = braces $ liftM TmRecord $ liftM (addNumbers 1) $ 
              sepBy parseRecordField comma
    where addNumbers _ [] = []
          addNumbers i (("-1",t):fs) = (show i, t) : (addNumbers (i+1) fs)
          addNumbers i (       f:fs) =           f : (addNumbers (i+1) fs)

parseRecordField = liftM2 (,) parseName parseTerm
    where parseName = (try (do {name <- identifier; symbol "="; return name}))
                      <|> return "-1"

parseProj = do t <- parseRecord <|> parseVar <|> parens parseTerm
               symbol "."
               liftM (TmProj t) parseFieldAccessor

parseFieldAccessor = identifier <|> 
                     liftM show natural

{- ------------------------------
   Putting it all together
   ------------------------------ -}

parseNonApp = parseAbs <|>
              (try parseProj) <|>
              parseVar <|>
              parseRecord <|>
              parens parseTerm

-- For non-applications, we don't need to deal with associativity,
-- but we need to special handling (in the form of 'chainl1' here)
-- so that we enforce left-associativity as we aggregate a list of terms
parseTerm = chainl1 parseNonApp $ return TmApp

parseTerms = do whiteSpace -- lexer handles whitespace everywhere except here
                ts <- endBy1 parseTerm semi
                eof
                return ts

parseRcdsub :: String -> ThrowsError [Term]
parseRcdsub str 
    = case runParser parseTerms newContext "rcdsub Parser" str of
        Left err -> throwError $ ParserError $ show err
        Right ts -> return ts

{- ------------------------------
   Helpers
   ------------------------------ -}

throwsToParser action = case action of
                          Left err  -> fail $ show err
                          Right val -> return val
