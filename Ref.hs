module Ref where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

fixLangDef = LanguageDef { 
    commentStart = [],
    commentEnd = [],
    commentLine = [],
    nestedComments = True,
    identStart = alphaNum,
    identLetter = alphaNum,
    opStart = opLetter fixLangDef,
    opLetter = oneOf ";:!&*+./<=>#?@^|-~_()[]",
    reservedOpNames = ["{","}"],
    reservedNames = [],
    caseSensitive = True
}

lexer = P.makeTokenParser fixLangDef

identifier= P.identifier lexer
symbol = P.symbol lexer
reservedOp = P.reservedOp lexer
operator = P.operator lexer

file :: Parser String
file = do
    name <- many1 alphaNum
    symbol "."
    ext <- many1 alphaNum
    return $ name++"."++ext

ref :: Parser (IO String)
ref = try $ do
    reservedOp "{"
    f <- file
    reservedOp "}"
    return $ do fs <- readFile f
                case parse solve f fs of
                    Right p -> do ps <- p
                                  return $ "("++ps++")"
                    Left e -> print e >> error ""

solve :: Parser (IO String)
solve = do
    P.whiteSpace lexer
    s <- many $ do
        s1 <- many1 $ noneOf ['{','}']
        P.whiteSpace lexer
        r <- option (return "") ref
        P.whiteSpace lexer
        return $ do {r1 <- r; return $ s1++r1}
    eof
    return $ do {ss <- sequence s; return $ concat ss}
