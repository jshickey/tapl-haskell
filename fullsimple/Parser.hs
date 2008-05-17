module Parser ( parseFullSimple ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Monad
import Control.Monad.Error

import Syntax
import TaplError

{- ------------------------------
   Lexer, making use of the Parsec.Token and Language
   modules for ease of lexing programming language constructs
   ------------------------------ -}
fullSimpleDef = LanguageDef
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = ""
                , nestedComments  = False
                , identStart      = letter <|> char '_'
                , identLetter     = letter <|> char '_' <|> digit
                , opStart         = fail "no operators"
                , opLetter        = fail "no operators"
                , reservedOpNames = []
                , caseSensitive   = True
                , reservedNames   = ["inert", "true", "false", "if", "then", "else", "Bool", "Nat", "String", "Unit", "Float", "case", "of", "as", "lambda", "let", "in", "fix", "letrec", "timesfloat", "succ", "pred", "iszero", "unit"]
                }

lexer = P.makeTokenParser fullSimpleDef

parens     = P.parens     lexer
braces     = P.braces     lexer
identifier = P.identifier lexer
reserved   = P.reserved   lexer
symbol     = P.symbol     lexer
whiteSpace = P.whiteSpace lexer
float      = P.float      lexer
semi       = P.semi       lexer

{- ------------------------------
   Parsing Binders
   ------------------------------ -}

parseBinder = do var <- identifier
                 symbol ":"
                 ty <- parseType
                 let binding = VarBind ty
                 updateState $ \ctx -> appendBinding var binding ctx
                 return $ TmBind var binding

{- ------------------------------
   Parsing Types
   ------------------------------ -}

parseTypeBool = reserved "Bool" >> return TyBool

parseTypeNat  = reserved "Nat"  >> return TyNat

parseType = parseTypeBool <|>
            parseTypeNat

{- ------------------------------
   Parsing zero-arg terms
   ------------------------------ -}

parseTrue  = reserved "true"  >> return TmTrue

parseFalse = reserved "false" >> return TmFalse

parseZero  = symbol "0"       >> return TmZero

{- ------------------------------
   Other Parsers
   ------------------------------ -}

indexOfForParser var ctx = case indexOf var ctx of
                             Left err -> fail $ show err
                             Right val -> return val

parseVar = do var <- identifier
              ctx <- getState
              idx <- indexOfForParser var ctx
              return $ TmVar idx (ctxLength ctx)

{- ------------------------------
   Putting it all together
   ------------------------------ -}

parseNonApp = parseTrue <|>
              parseFalse <|>
              parseZero <|>
              (try parseBinder) <|>
              parseVar 

parseTerm = parseNonApp

parseTerms = do whiteSpace -- lexer handles whitespace everywhere except here
                ts <- endBy1 parseTerm semi
                eof
                return ts

parseFullSimple :: String -> ThrowsError [Term]
parseFullSimple str 
    = case runParser parseTerms newContext "fullsimple Parser" str of
        Left err -> throwError $ ParserError $ show err
        Right ts -> return ts