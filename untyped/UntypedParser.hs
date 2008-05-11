{- Uses Parsec to parse untyped lambda calculus, making use of the
   LanguageDef functionality to simplify parsing (e.g., we get
   comment-parsing for free).  

 -}
module UntypedParser (parseUntyped) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Control.Monad
import Control.Monad.Error
    
import Untyped
import TaplError
import Context

nonleadingLetters = letter <|> digit <|> oneOf "'*&^%$"

untypedDef = LanguageDef
             { commentStart = "/*"
             , commentEnd = "*/"
             , commentLine = ""
             , nestedComments = False
             , identStart = letter
             , identLetter = nonleadingLetters
             , opStart = letter
             , opLetter = nonleadingLetters
             , reservedNames = ["lambda", "/"]
             , reservedOpNames = []
             , caseSensitive = True
             }
               
lexer = P.makeTokenParser untypedDef

whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer
parens = P.parens lexer
dot = P.dot lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

-- binds this variable into the context
binder = do var <- identifier
            reserved "/"
            updateState (\c -> appendBinding c var)
            return $ TmBind var

-- Returns the index of a variable in a context.
-- Returns a GenParser, even though no parsing is done, so that
-- we can use Parsec's error handling
getIndex :: Context -> String -> GenParser Char Context Int
getIndex (Context boundNames) name = iter 0 (reverse boundNames)
    where iter _ [] = fail $ "Unbound name: " ++ name
          iter c (n:ns) | n == name = return c
                        | otherwise = iter (c + 1) ns

untypedVar = do var <- identifier
                ctx <- getState
                idx <- getIndex ctx var
                return $ TmVar { index = idx, contextLength = ctxLength ctx }

-- parses, e.g., "lambda x. x x"
untypedAbs = do reserved "lambda"
                var <- identifier
                dot
                -- bind the variable before parsing the body of the 
                -- lambda abstraction
                ctx <- getState
                setState $ appendBinding ctx var

                term <- parseTerm

                -- now that we're leaving this lambda abstraction, we need
                -- to restore the original context, before "var" was bound
                setState ctx
                return $ TmAbs var term
                
-- Parses everything expect for an application, which we handle 
-- separately to handle associativity (and to avoid infinite loops!)
parseNonApp = try binder
            <|> untypedVar
            <|> untypedAbs
            <|> parens parseTerm

-- chainl1 gives us left-associativity of applications
parseTerm = chainl1 parseNonApp (return TmApp)

parseStmt = do t <- parseTerm
               semi
               return t

untypedParser = do setState newContext
                   whiteSpace
                   terms <- many1 parseStmt
                   return terms

parseUntyped :: String -> ThrowsError [Term]
parseUntyped str = case runParser untypedParser newContext "Untyped Parser" str of
                     Left err  -> throwError $ ParserError $ show err
                     Right val -> return val