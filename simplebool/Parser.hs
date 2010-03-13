{- Uses Parsec to parse simplebool, making use of the
   LanguageDef functionality to simplify parsing (e.g., we get
   comment-parsing for free).  

 -}
module Parser (parseSimpleBool) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Control.Monad
import Control.Monad.Error
    
import SimpleBool
import TaplError
import Context

nonleadingLetters = letter <|> digit <|> oneOf "'*&^%$"

simpleBoolDef = LanguageDef
             { commentStart = "/*"
             , commentEnd = "*/"
             , commentLine = ""
             , nestedComments = False
             , identStart = letter
             , identLetter = nonleadingLetters
             , opStart = letter
             , opLetter = nonleadingLetters
             , reservedNames = ["lambda", "if", "then", "else", 
                                "true", "false", "Bool"]
             , reservedOpNames = []
             , caseSensitive = True
             }
               
lexer = P.makeTokenParser simpleBoolDef

whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer
parens = P.parens lexer
dot = P.dot lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

-- binds this variable into the context, along with its explicitly 
-- declared type
binder = do var <- identifier
            symbol ":"
            ty <- parseType
            let binding = VarBind ty
            updateState (\c -> appendBinding c var binding)
            return $ TmBind var binding

-- Returns the index of a variable in a context.
-- Returns a GenParser, even though no parsing is done, so that
-- we can use Parsec's error handling
getIndex :: Context -> String -> GenParser Char Context Int
getIndex (Context boundNames) name = iter 0 (reverse boundNames)
    where iter _ [] = fail $ "Unbound name: " ++ name
          iter c ((n,_):ns) | n == name = return c
                            | otherwise = iter (c + 1) ns

parseTrue = reserved "true" >> return TmTrue

parseFalse = reserved "false" >> return TmFalse

parseIf = do reserved "if"
             pred <- parseTerm
             reserved "then"
             conseq <- parseTerm
             reserved "else"
             alt <- parseTerm
             return $ TmIf pred conseq alt

parseVar = do var <- identifier
              ctx <- getState
              idx <- getIndex ctx var
              return $ TmVar { index = idx, contextLength = ctxLength ctx }

parseBoolTy = reserved "Bool" >> return TyBool

parseAtomicType = parens parseType <|> 
                  parseBoolTy

-- chainr1 gives us right-associativity of types
parseType = chainr1 parseAtomicType (symbol "->" >> return TyArr)

-- parses, e.g., "lambda x:Bool. x x"
parseAbs = do reserved "lambda"
              ctx <- getState
              (TmBind var (VarBind ty)) <- binder
              dot
              term <- parseTerm
              -- now that we're leaving this lambda abstraction, we need
              -- to restore the original context, before "var" was bound
              setState ctx
              return $ TmAbs var ty term
                
-- Parses everything expect for an application, which we handle 
-- separately to handle associativity (and to avoid infinite loops!)
parseNonApp = try binder
            <|> parseVar
            <|> parseAbs
            <|> parseTrue
            <|> parseFalse
            <|> parseIf
            <|> parens parseTerm

-- chainl1 gives us left-associativity of applications
parseTerm = chainl1 parseNonApp (return TmApp)

parseStmt = do t <- parseTerm
               semi
               return t

simpleBoolParser = do setState newContext
                      whiteSpace
                      terms <- many1 parseStmt
                      return terms

parseSimpleBool :: String -> ThrowsError [Term]
parseSimpleBool str = case runParser simpleBoolParser newContext "SimpleBool Parser" str of
                     Left err  -> throwError $ ParserError $ show err
                     Right val -> return val