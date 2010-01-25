module GenParser (genParser) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genParser :: Config -> IOThrowsError ()
genParser c =
    lift $ writeToFile "Parser.hs" $
    base ++
    beginLangDef ++
    (if (useIsorec c) then isorecKeywords else "") ++
    (if (useEquirec c) then equirecKeywords else "") ++
    endLangDef ++
    lexer ++
    parseBasicTypes ++
    (if ((useIsorec c) || (useEquirec c)) then parseRecType else "") ++
    beginParseTypeArr ++
    (if ((useIsorec c) || (useEquirec c)) then recParseTypeArr else "") ++
    endParseTypeArr ++
    parseBasicTerms ++
    (if (useIsorec c) then parseFold else "") ++
    beginParseNonApp ++
    (if (useIsorec c) then foldParseNonApp else "") ++
    endParseNonApp ++
    end

base = "{- Parsec parser for fullsimple.  The sole expected method, parseFullSimple,\n\
\   takes a string as input, and returns a list of terms, where each term\n\
\   was separated by a semicolon in the input.\n\
\ -}\n\
\module Parser ( parseFullSimple ) where\n\
\\n\
\import Text.ParserCombinators.Parsec\n\
\import qualified Text.ParserCombinators.Parsec.Token as P\n\
\import Text.ParserCombinators.Parsec.Language\n\
\import Control.Monad\n\
\import Control.Monad.Error\n\
\import Control.Monad.State\n\
\import Data.Char\n\
\\n\
\import Syntax\n\
\import Typing\n\
\import TaplError\n\
\import SimpleContext\n\
\\n"

beginLangDef = "{- ------------------------------\n\
\   Lexer, making use of the Parsec.Token and Language\n\
\   modules for ease of lexing programming language constructs\n\
\   ------------------------------ -}\n\
\fullSimpleDef = LanguageDef\n\
\                { commentStart    = \"/*\"\n\
\                , commentEnd      = \"*/\"\n\
\                , commentLine     = \"\"\n\
\                , nestedComments  = False\n\
\                , identStart      = letter \n\
\                , identLetter     = letter <|> digit\n\
\                , opStart         = fail \"no operators\"\n\
\                , opLetter        = fail \"no operators\"\n\
\                , reservedOpNames = []\n\
\                , caseSensitive   = True\n\
\                , reservedNames   = [\"inert\", \"true\", \"false\", \"if\", \"then\", \"else\", \"Bool\", \"Nat\", \"String\", \"Unit\", \"Float\", \"case\", \"of\", \"as\", \"lambda\", \"let\", \"in\", \"fix\", \"letrec\", \"timesfloat\", \"succ\", \"pred\", \"iszero\", \"unit\""
isorecKeywords = ", \"Rec\", \"fold\", \"unfold\""
equirecKeywords = ", \"Rec\""
                  
endLangDef = " ]\n\
\                }\n\
\\n"

lexer = "lexer = P.makeTokenParser fullSimpleDef\n\
\\n\
\parens        = P.parens        lexer\n\
\braces        = P.braces        lexer\n\
\brackets      = P.brackets      lexer\n\
\squares       = P.squares       lexer\n\
\identifier    = P.identifier    lexer\n\
\reserved      = P.reserved      lexer\n\
\symbol        = P.symbol        lexer\n\
\whiteSpace    = P.whiteSpace    lexer\n\
\float         = P.float         lexer\n\
\semi          = P.semi          lexer\n\
\comma         = P.comma         lexer\n\
\colon         = P.colon         lexer\n\
\stringLiteral = P.stringLiteral lexer\n\
\natural       = P.natural       lexer\n\
\\n"
 
parseBasicTypes = "{- ------------------------------\n\
\   Parsing Binders\n\
\   ------------------------------ -}\n\
\\n\
\-- due to the definition of \"identState\" in fullSimpleDef,\n\
\-- this is the only way that an underscore can enter our system,\n\
\-- and thus there is no chance of it being misused as a variable elsewhere\n\
\parseVarBind = do var <- identifier <|> symbol \"_\"\n\
\                  symbol \":\"\n\
\                  ty <- parseType\n\
\                  let binding = VarBind ty\n\
\                  updateState $ appendBinding var binding\n\
\                  return $ TmBind var binding\n\
\\n\
\\n\
\parseAbbBind forLetrec\n\
\    = do var <- identifier <|> symbol \"_\"\n\
\         -- For a letrec, we need to temporarily add a binding, so that\n\
\         -- we can lookup this variable while parsing the body.  \n\
\         -- Note that both setState calls use the original Context\n\
\         ctx <- getState\n\
\         when forLetrec $ setState $ appendBinding var NameBind ctx\n\
\         binding <- getBinding var\n\
\         setState $ appendBinding var binding ctx\n\
\         return $ TmBind var binding\n\
\    where getBinding var = if (isUpper $ var !! 0)\n\
\                           then (try $ completeTyAbbBind var) <|>\n\
\                                (return TyVarBind)\n\
\                           else withType <|> withoutType\n\
\          withoutType    = do symbol \"=\"\n\
\                              t <- parseTerm\n\
\                              liftM (TmAbbBind t) (getType t)\n\
\          withType       = do symbol \":\"\n\
\                              ty <- parseType\n\
\                              symbol \"=\"\n\
\                              liftM ((flip TmAbbBind) (Just ty)) parseTerm\n\
\          completeTyAbbBind var \n\
\                         = do symbol \"=\"\n\
\                              ty <- parseType\n\
\                              return $ TyAbbBind ty\n\
\          getType t      = do ctx <- getState\n\
\                              case evalState (runErrorT (typeof t)) ctx of\n\
\                                Left err -> return Nothing\n\
\                                Right ty -> return $ Just ty\n\
\\n\
\parseBinder = (try parseVarBind) <|> (parseAbbBind False)\n\
\\n\
\{- ------------------------------\n\
\   Parsing Types\n\
\   ------------------------------ -}\n\
\\n\
\parseTypeBool   = reserved \"Bool\"   >> return TyBool\n\
\\n\
\parseTypeNat    = reserved \"Nat\"    >> return TyNat\n\
\\n\
\parseTypeFloat  = reserved \"Float\"  >> return TyFloat\n\
\\n\
\parseTypeUnit   = reserved \"Unit\"   >> return TyUnit\n\
\\n\
\parseTypeString = reserved \"String\" >> return TyString\n\
\\n\
\parseNamedType  = do ty <- identifier\n\
\                     if isUpper $ ty !! 0\n\
\                       then makeNamedType ty\n\
\                       else fail \"types must start with an uppercase letter\"\n\
\    where makeNamedType ty       = do ctx <- getState \n\
\                                      throwsToParser $ makeTyVarOrTyId ty ctx\n\
\          makeTyVarOrTyId ty ctx = catchError (makeTyVar ty ctx) \n\
\                                   (\\e -> return $ TyId ty)\n\
\          makeTyVar       ty ctx = do idx <- indexOf ty ctx\n\
\                                      return $ TyVar $ TmVar idx (ctxLength ctx)\n\
\\n\
\parseVariantType = do symbol \"<\"\n\
\                      fields <- sepBy1 parseField comma\n\
\                      symbol \">\"\n\
\                      return $ TyVariant fields\n\
\    where parseField = do var <- identifier\n\
\                          colon\n\
\                          ty <- parseType\n\
\                          return (var, ty)\n\
\\n\
\parseTypeTop = reserved \"Top\" >> return TyTop\n\
\\n\
\parseTypeBot = reserved \"Bot\" >> return TyBot\n\
\\n\
\parseTypeRecord = braces $ liftM TyRecord $ sepBy parseField comma\n\
\    where parseField = do a <- parseFieldAccessor\n\
\                          symbol \":\"\n\
\                          ty <- parseType\n\
\                          return (a,ty)\n\
\\n"

parseRecType = "parseRecType = do reserved \"Rec\"\n\
\                  nt <- parseNamedType\n\
\                  symbol \".\"\n\
\                  ty <- parseType\n\
\                  return $ TyRec nt ty\n\
\\n"

beginParseTypeArr = "parseTypeArr = parseTypeBool   <|>\n\
\               parseTypeNat    <|>\n\
\               parseTypeFloat  <|>\n\
\               parseTypeUnit   <|>\n\
\               parseTypeString <|>\n\
\               parseTypeTop    <|>\n\
\               parseTypeBot    <|>\n\
\               parseTypeRecord <|>\n\
\               parseNamedType  <|>\n\
\               parseVariantType  <|>\n"

recParseTypeArr = "               parseRecType  <|>\n"
                                        
endParseTypeArr = "               parens parseType\n\
\\n\
\parseType = parseTypeArr `chainr1` (symbol \"->\" >> return TyArr)\n\
\\n"

parseBasicTerms = "{- ------------------------------\n\
\   Parsing zero-arg terms\n\
\   ------------------------------ -}\n\
\\n\
\parseTrue  = reserved \"true\"  >> return TmTrue\n\
\\n\
\parseFalse = reserved \"false\" >> return TmFalse\n\
\\n\
\parseUnit  = reserved \"unit\"  >> return TmUnit\n\
\\n\
\parseNat = liftM numToSucc natural\n\
\    where numToSucc 0 = TmZero\n\
\          numToSucc n = TmSucc $ numToSucc (n - 1)\n\
\\n\
\{- ------------------------------\n\
\   Arith Parsers\n\
\   ------------------------------ -}\n\
\\n\
\parseOneArg keyword constructor = reserved keyword >> \n\
\                                  liftM constructor parseTerm\n\
\\n\
\parseSucc   = parseOneArg \"succ\"   TmSucc\n\
\\n\
\parsePred   = parseOneArg \"pred\"   TmPred\n\
\\n\
\parseIsZero = parseOneArg \"iszero\" TmIsZero\n\
\\n\
\{- ------------------------------\n\
\   Other Parsers\n\
\   ------------------------------ -}\n\
\\n\
\parseString = liftM TmString stringLiteral \n\
\\n\
\parseFloat = liftM TmFloat float\n\
\\n\
\parseTimesFloat = reserved \"timesfloat\" >> \n\
\                  liftM2 TmTimesFloat parseNonApp parseNonApp\n\
\\n\
\parseIf = do reserved \"if\"\n\
\             t1 <- parseTerm\n\
\             reserved \"then\"\n\
\             t2 <- parseTerm\n\
\             reserved \"else\"\n\
\             liftM (TmIf t1 t2) parseTerm\n\
\\n\
\parseVar = do var <- identifier\n\
\              if (isUpper $ var !! 0)\n\
\                then fail \"variables must start with a lowercase letter\"\n\
\                else do ctx <- getState\n\
\                        idx <- throwsToParser $ indexOf var ctx\n\
\                        return $ TmVar idx (ctxLength ctx)\n\
\\n\
\parseInert = reserved \"inert\" >> squares (liftM TmInert parseType)\n\
\\n\
\{- ------------------------------\n\
\   let/lambda\n\
\   ------------------------------ -}\n\
\\n\
\-- for both let and lambda, we need to make sure we restore the\n\
\-- state after parsing the body, so that the lexical binding doesn't leak\n\
\parseAbs = do reserved \"lambda\"\n\
\              ctx <- getState\n\
\              (TmBind var (VarBind ty)) <- parseVarBind\n\
\              symbol \".\"\n\
\              body <- parseTerm\n\
\              setState ctx\n\
\              return $ TmAbs var ty body\n\
\\n\
\parseLet = do reserved \"let\"\n\
\              ctx <- getState\n\
\              (TmBind var binding) <- (parseAbbBind False)\n\
\              reserved \"in\"\n\
\              body <- parseTerm\n\
\              setState ctx\n\
\              case binding of\n\
\                TmAbbBind t ty -> return $ TmLet var t body\n\
\                otherwise      -> fail \"malformed let statement\"\n\
\\n\
\{- ------------------------------\n\
\   Fix and Letrec\n\
\   ------------------------------ -}\n\
\\n\
\parseLetrec = do reserved \"letrec\"\n\
\                 ctx <- getState\n\
\                 (TmBind var binding) <- (parseAbbBind True)\n\
\                 reserved \"in\"\n\
\                 body <- parseTerm\n\
\                 setState ctx\n\
\                 case binding of\n\
\                   TmAbbBind t (Just ty)\n\
\                       -> return $ TmLet var (TmFix (TmAbs var ty t)) body\n\
\                   otherwise      \n\
\                       -> fail \"malformed letrec statement\"\n\
\\n\
\parseFix = reserved \"fix\" >> liftM TmFix parseTerm\n\
\\n\
\{- ------------------------------\n\
\   Records and Projections\n\
\   ------------------------------ -}\n\
\\n\
\-- Fields can either be named or not.  If they are not, then they\n\
\-- are numbered starting with 1.  To keep parsing the fields simple,\n\
\-- we label them with -1 at first if they have no name.  We then\n\
\-- replace the -1's with the correct index as a post-processing step.\n\
\parseRecord = braces $ liftM TmRecord $ liftM (addNumbers 1) $ \n\
\              sepBy parseRecordField comma\n\
\    where addNumbers _ [] = []\n\
\          addNumbers i ((\"-1\",t):fs) = (show i, t) : (addNumbers (i+1) fs)\n\
\          addNumbers i (       f:fs) =           f : (addNumbers (i+1) fs)\n\
\\n\
\parseRecordField = liftM2 (,) parseName parseTerm\n\
\    where parseName = (try (do {name <- identifier; symbol \"=\"; return name}))\n\
\                      <|> return \"-1\"\n\
\\n\
\parseProj = do t <- parseRecord <|> parseVar <|> parens parseTerm\n\
\               symbol \".\"\n\
\               liftM (TmProj t) parseFieldAccessor\n\
\\n\
\parseFieldAccessor = identifier <|> \n\
\                     liftM show natural\n\
\\n\
\{- ------------------------------\n\
\   Variants and Cases\n\
\   ------------------------------ -}\n\
\\n\
\parseVariant = do symbol \"<\"\n\
\                  var <- identifier\n\
\                  symbol \"=\"\n\
\                  t <- parseTerm\n\
\                  symbol \">\"\n\
\                  reserved \"as\"\n\
\                  liftM (TmTag var t) parseType\n\
\\n\
\parseCase = do reserved \"case\"\n\
\               t <- parseTerm\n\
\               reserved \"of\"\n\
\               liftM (TmCase t) $ sepBy1 parseBranch (symbol \"|\")\n\
\    where parseBranch = do symbol \"<\"\n\
\                           label <- identifier\n\
\                           symbol \"=\"\n\
\                           var <- identifier\n\
\                           symbol \">\"\n\
\                           symbol \"==>\"\n\
\                           ctx <- getState\n\
\                           setState $ appendBinding var NameBind ctx\n\
\                           t <- parseTerm\n\
\                           setState ctx\n\
\                           return (label, (var,t))\n\
\\n"

parseFold = "{- ------------------------------\n\
\   Fold and Unfold\n\
\   ------------------------------ -}\n\
\\n\
\foldHelper keyword cons = do reserved keyword\n\
\                             ty <- brackets parseType\n\
\                             t <- parseTerm\n\
\                             return $ cons ty t\n\
\\n\ 
\parseFold = foldHelper \"fold\" TmFold\n\
\parseUnfold = foldHelper \"unfold\" TmUnfold\n\
\\n"  

beginParseNonApp = "{- ------------------------------\n\
\   Putting it all together\n\
\   ------------------------------ -}\n\
\\n\
\parseNonApp = parseTrue <|>\n\
\              parseFalse <|>\n\
\              parseSucc <|>\n\
\              parsePred <|>\n\
\              parseIsZero <|>\n\
\              parseIf <|>\n\
\              (try parseFloat) <|>\n\
\              parseTimesFloat <|>\n\
\              parseNat <|>\n\
\              parseAbs <|>\n\
\              parseLet <|>\n\
\              (try parseProj) <|>\n\
\              parseRecord <|>\n\
\              (try parseBinder) <|>\n\
\              parseVar <|>\n\
\              parseUnit <|>\n\
\              parseString <|>\n\
\              parseCase <|>\n\
\              parseVariant <|>\n\
\              parseInert <|>\n\
\              parseFix <|>\n\
\              parseLetrec <|>\n"

foldParseNonApp = "              parseFold <|>\n\
\              parseUnfold <|>\n"
               
endParseNonApp = "              parens parseTerm\n"

end = "\n\
\-- parses a non-application which could be an ascription\n\
\-- (the non-application parsing is left-factored)\n\
\parseNonAppOrAscribe = do t <- parseNonApp\n\
\                          (do reserved \"as\"\n\
\                              ty <- parseType\n\
\                              return $ TmAscribe t ty) <|> return t\n\
\\n\
\-- For non-applications, we don't need to deal with associativity,\n\
\-- but we need to special handling (in the form of 'chainl1' here)\n\
\-- so that we enforce left-associativity as we aggregate a list of terms\n\
\parseTerm = chainl1 parseNonAppOrAscribe $ return TmApp\n\
\\n\
\parseTerms = do whiteSpace -- lexer handles whitespace everywhere except here\n\
\                ts <- endBy1 parseTerm semi\n\
\                eof\n\
\                return ts\n\
\\n\
\parseFullSimple :: String -> ThrowsError [Term]\n\
\parseFullSimple str \n\
\    = case runParser parseTerms newContext \"fullsimple Parser\" str of\n\
\        Left err -> throwError $ ParserError $ show err\n\
\        Right ts -> return ts\n\
\\n\
\{- ------------------------------\n\
\   Helpers\n\
\   ------------------------------ -}\n\
\\n\
\throwsToParser action = case action of\n\
\                          Left err  -> fail $ show err\n\
\                          Right val -> return val\n"