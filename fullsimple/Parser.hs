{- Parsec parser for fullsimple.  The sole expected method, parseFullSimple,
   takes a string as input, and returns a list of terms, where each term
   was separated by a semicolon in the input.
 -}
module Parser ( parseFullSimple ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Monad
import Control.Monad.Error

import Syntax
import TaplError
import Context

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

parens        = P.parens        lexer
braces        = P.braces        lexer
identifier    = P.identifier    lexer
reserved      = P.reserved      lexer
symbol        = P.symbol        lexer
whiteSpace    = P.whiteSpace    lexer
float         = P.float         lexer
semi          = P.semi          lexer
stringLiteral = P.stringLiteral lexer
{- ------------------------------
   Parsing Binders
   ------------------------------ -}

parseVarBind = do var <- identifier
                  symbol ":"
                  ty <- parseType
                  let binding = VarBind ty
                  updateState $ \ctx -> appendBinding var binding ctx
                  return $ TmBind var binding

parseBinder = parseVarBind

{- ------------------------------
   Parsing Types
   ------------------------------ -}

parseTypeBool   = reserved "Bool"   >> return TyBool

parseTypeNat    = reserved "Nat"    >> return TyNat

parseTypeFloat  = reserved "Float"  >> return TyFloat

parseTypeUnit   = reserved "Unit"   >> return TyUnit

parseTypeString = reserved "String" >> return TyString

parseType = parseTypeBool   <|>
            parseTypeNat    <|>
            parseTypeFloat  <|>
            parseTypeUnit   <|>
            parseTypeString 

{- ------------------------------
   Parsing zero-arg terms
   ------------------------------ -}

parseTrue  = reserved "true"  >> return TmTrue

parseFalse = reserved "false" >> return TmFalse

parseZero  = symbol "0"       >> return TmZero

parseUnit  = reserved "unit"  >> return TmUnit

{- ------------------------------
   Arith Parsers
   ------------------------------ -}

parseOneArg keyword constructor = reserved keyword >> 
                                  liftM constructor parseTerm

parseSucc   = parseOneArg "succ"   TmSucc

parsePred   = parseOneArg "pred"   TmPred

parseIsZero = parseOneArg "iszero" TmIsZero

{- ------------------------------
   Other Parsers
   ------------------------------ -}

parseString = liftM TmString stringLiteral 

parseFloat = liftM TmFloat float

parseTimesFloat = reserved "timesfloat" >> 
                  liftM2 TmTimesFloat parseNonApp parseNonApp

parseIf = do reserved "if"
             t1 <- parseTerm
             reserved "then"
             t2 <- parseTerm
             reserved "else"
             liftM (TmIf t1 t2) parseTerm

indexOfForParser var ctx = case indexOf var ctx of
                             Left err -> fail $ show err
                             Right val -> return val

parseVar = do var <- identifier
              ctx <- getState
              idx <- indexOfForParser var ctx
              return $ TmVar idx (ctxLength ctx)

parseAbs = do reserved "lambda"
              (TmBind var (VarBind ty)) <- parseVarBind
              symbol "."
              liftM (TmAbs var ty) parseTerm

{- ------------------------------
   Putting it all together
   ------------------------------ -}

parseNonApp = parseTrue <|>
              parseFalse <|>
              parseZero <|>
              parseSucc <|>
              parsePred <|>
              parseIsZero <|>
              parseIf <|>
              parseFloat <|>
              parseTimesFloat <|>
              parseAbs <|>
              (try parseBinder) <|>
              parseVar <|>
              parseUnit <|>
              parseString <|>
              parens parseTerm

-- For non-applications, we don't need to deal with associativity,
-- but we need to special handling (in the form of 'chainl1' here)
-- so that we enforce left-associativity as we aggregate a list of terms
parseTerm = chainl1 parseNonApp $ return TmApp

parseTerms = do whiteSpace -- lexer handles whitespace everywhere except here
                ts <- endBy1 parseTerm semi
                eof
                return ts

parseFullSimple :: String -> ThrowsError [Term]
parseFullSimple str 
    = case runParser parseTerms newContext "fullsimple Parser" str of
        Left err -> throwError $ ParserError $ show err
        Right ts -> return ts