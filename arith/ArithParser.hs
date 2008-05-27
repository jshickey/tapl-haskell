module ArithParser (parseArith) where

import Text.ParserCombinators.Parsec
import Control.Monad.Error

import Arith
import TaplError

{- Parses a file contain expressions of the Arith language.  
 - Each term is separated by a semi-colon, and comments are allowed.
 - See test.f for an example file.
 - 
 - It would be simpler to use Parsec's LanguageDef functionality,
 - but I'll learn more by just using the basic Parsec functionality...
 -}

-- does not allow nested comments
comment = do string "/*"
             many ((noneOf "*") <|> starNotCommentEnd)
             commentEnd
             return ()

-- We need to use "try", because we want this to fail when
-- it encounters the end of the comment, and give back the
-- last star.  "commentEnd" below handles the case of extra
-- stars before the final "*/"
starNotCommentEnd = try (many1 (char '*') >>
                         noneOf "/")

commentEnd = many1 (char '*') >>
             char '/'

separator = (space >> return ()) <|> 
            comment

separators = many separator
             
separators1 = many1 separator

-- --------------------
-- Zero-arg terms

parseTrue = string "true" >> return TmTrue

parseFalse = string "false" >> return TmFalse

parseNat = liftM (numToSucc . read) $ many1 digit
    where numToSucc 0 = TmZero
          numToSucc n = TmSucc $ numToSucc (n - 1)

-- --------------------
-- One-arg terms

parseOneArgTerm keyword constructor = 
    string keyword >> separators1 >> liftM constructor parseTerm

parseSucc = parseOneArgTerm "succ" TmSucc

parsePred = parseOneArgTerm "pred" TmPred

parseIsZero = parseOneArgTerm "iszero" TmIsZero

surroundedBy open close parser = do open
                                    separators
                                    output <- parser
                                    separators
                                    close
                                    return output
-- --------------------
-- if-then-else, and the rest of the parser

parseIf = do string "if"
             separators1
             pred <- parseTerm
             separators1
             string "then"
             separators1
             conseq <- parseTerm
             separators1
             string "else"
             separators1
             alt <- parseTerm
             return $ TmIf pred conseq alt

parseTerm = parseTrue <|>
            parseFalse <|>
            (try parseIf) <|> -- use try, due to "iszero" below
            parseSucc <|>
            parsePred <|>
            parseIsZero <|>
            parseNat <|>
            surroundedBy (char '(') (char ')') parseTerm

termSep = separators >> char ';' >> separators

arithParser :: Parser [Term]
arithParser = separators >>
              endBy1 parseTerm termSep

parseArith :: String -> ThrowsError [Term]
parseArith str = case (parse arithParser "ArithParser" str) of
                   Left err -> throwError $ ParserError $ show err
                   Right terms -> return terms
