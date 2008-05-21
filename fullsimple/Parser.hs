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
import Control.Monad.State
import Data.Char

import Syntax
import Typing
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
                , identStart      = letter 
                , identLetter     = letter <|> digit
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

-- due to the definition of "identState" in fullSimpleDef,
-- this is the only way that an underscore can enter our system,
-- and thus there is no chance of it being misused as a variable elsewhere
parseVarBind = do var <- identifier <|> symbol "_"
                  symbol ":"
                  ty <- parseType
                  let binding = VarBind ty
                  updateState $ appendBinding var binding
                  return $ TmBind var binding

parseAbbBind = do var <- identifier <|> symbol "_"
                  binding <- getBinding var
                  updateState $ appendBinding var binding
                  return $ TmBind var binding
    where getBinding var
                = if (isUpper $ var !! 0)
                    then (try $ completeTyAbbBind var) <|>
                         (return TyVarBind)
                    else do symbol "="
                            t <- parseTerm
                            ty <- getType t
                            return $ TmAbbBind t ty
          completeTyAbbBind var = do symbol "="
                                     ty <- parseType
                                     return $ TyAbbBind ty
          getType t = do ctx <- getState
                         case evalState (runErrorT (typeof t)) ctx of
                           Left err -> return Nothing
                           Right ty -> return $ Just ty

parseBinder = (try parseVarBind) <|> parseAbbBind

{- ------------------------------
   Parsing Types
   ------------------------------ -}

parseTypeBool   = reserved "Bool"   >> return TyBool

parseTypeNat    = reserved "Nat"    >> return TyNat

parseTypeFloat  = reserved "Float"  >> return TyFloat

parseTypeUnit   = reserved "Unit"   >> return TyUnit

parseTypeString = reserved "String" >> return TyString

parseNamedType  = do ty <- identifier
                     if isUpper $ ty !! 0
                       then makeNamedType ty
                       else fail "types must start with an uppercase letter"
    where makeNamedType ty       = do ctx <- getState 
                                      throwsToParser $ makeTyVarOrTyId ty ctx
          makeTyVarOrTyId ty ctx = catchError (makeTyVar ty ctx) 
                                   (\e -> return $ TyId ty)
          makeTyVar       ty ctx = do idx <- indexOf ty ctx
                                      return $ TyVar $ TmVar idx (ctxLength ctx)

parseFieldType  = fail "todo"

parseTypeArr = parseTypeBool   <|>
               parseTypeNat    <|>
               parseTypeFloat  <|>
               parseTypeUnit   <|>
               parseTypeString <|>
               parseNamedType  <|>
               parseFieldType  <|>
               braces parseType

parseType = parseTypeArr `chainr1` (symbol "->" >> return TyArr)

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

parseVar = do var <- identifier
              if (isUpper $ var !! 0)
                then fail "variables must start with a lowercase letter"
                else do ctx <- getState
                        idx <- throwsToParser $ indexOf var ctx
                        return $ TmVar idx (ctxLength ctx)

parseAbs = do reserved "lambda"
              (TmBind var (VarBind ty)) <- parseVarBind
              symbol "."
              liftM (TmAbs var ty) parseTerm

parseLet = do reserved "let"
              (TmBind var binding) <- parseAbbBind
              reserved "in"
              body <- parseTerm
              case binding of
                TmAbbBind t ty -> return $ TmLet var t body
                otherwise      -> fail "malformed let statement"

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
              parseLet <|>
              (try parseBinder) <|>
              parseVar <|>
              parseUnit <|>
              parseString <|>
              parens parseTerm

-- parses a non-application which could be an ascription
-- (the non-application parsing is left-factored)
parseNonAppOrAscribe = do t <- parseNonApp
                          (do reserved "as"
                              ty <- parseType
                              return $ TmAscribe t ty) <|> return t

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

{- ------------------------------
   Helpers
   ------------------------------ -}

throwsToParser action = case action of
                          Left err  -> fail $ show err
                          Right val -> return val
