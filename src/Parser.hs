module Parser (Parser.parse) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.List
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Ast

-- Low level parsers

tokenSymbol :: String -> Parser String
tokenSymbol s = try (string s) <* spaces

tokenWord :: String -> Parser String
tokenWord s = try (string s) <* notFollowedBy alphaNum <* spaces

identifier :: Parser String
identifier = (((:) <$> letter <*> many alphaNum) <|> between (char '`') (char '`') (many1 (noneOf "`"))) <* spaces <?> "identifier"

-- http://stackoverflow.com/questions/10726085
pinteger :: Parser Integer
pinteger = (foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit) <* spaces <?> "integer"

pint :: Parser Int
pint = (foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit) <* spaces <?> "int"

pboolean :: Parser Bool
pboolean = (const True <$> tokenWord "true") <|> (const False <$> tokenWord "false") <?> "boolean"

-- Medium level parsers

parentheses :: Parser a -> Parser a
parentheses = between (tokenSymbol "(") (tokenSymbol ")")

commaList :: Parser a -> Parser [a]
commaList = flip sepBy (tokenSymbol ",")

parenCommas :: Parser a -> Parser [a]
parenCommas = parentheses . commaList

semicolon :: Parser String
semicolon = tokenWord ";"

-- High level parsers

parseType :: Parser Type
parseType = foldl FunctionType <$> basic <*> many (parenCommas parseType) <?> "type"
  where basic = (const Var <$> tokenWord "#")
            <|> (IntType <$> (char 'i' *> pint))
            <|> (FloatType <$> (char 'f' *> pint))
            <|> (const VoidType <$> tokenWord "void")
            <|> (UnknownType <$> identifier)

primary :: Parser Expression
primary = (ConstantInteger (IntType 32) <$> pinteger)
      <|> ((\b -> ConstantInteger (IntType 1) (if b then 1 else 0)) <$> pboolean)
      <|> (Identifier Var <$> identifier)
      <|> parentheses parseExpr
      <?> "value"

parseExpr :: Parser Expression
parseExpr = buildExpressionParser table primary
  where table = [[Postfix (flip MethodCall <$> parenCommas parseExpr <?> "function application")]
                ,[Postfix (Cast <$> (tokenSymbol "@" *> parseType))]
                ,[prefix "-" Negation]
                ,[binary "*" (BinaryOp Multiplication) AssocLeft, binary "/" (BinaryOp Division) AssocLeft]
                ,[binary "+" (BinaryOp Addition) AssocLeft, binary "-" (BinaryOp Subtraction) AssocLeft]
                ,[binary "<<" (BinaryOp ShiftLeft) AssocLeft, binary ">>" (BinaryOp ShiftRight) AssocLeft]
                ,[binary "&" (BinaryOp And) AssocLeft]
                ,[binary "|" (BinaryOp Or) AssocLeft]
                ,[binary "=" Assignment AssocRight]
                ]
        binary name fun = Infix (tokenSymbol name >> return fun)
        prefix name fun = Prefix (tokenSymbol name >> return fun)

parseIf :: Parser Statement
parseIf = IfStatement <$> (tokenWord "if" *> parseExpr) <*> parseBlock <*> ((tokenWord "else" *> parseBlock) <|> return [])

parseWhile :: Parser Statement
parseWhile = WhileStatement <$> (tokenWord "while" *> parseExpr) <*> parseBlock

parseReturn :: Parser Statement
parseReturn = (Return . Just) <$> (tokenWord "return" *> parseExpr <* semicolon)

parseStatement :: Parser Statement
parseStatement = parseIf <|> parseWhile <|> parseReturn <|> (ExprStatement <$> parseExpr <* semicolon)

parseBlock :: Parser Block
parseBlock = between (tokenSymbol "{") (tokenSymbol "}") (many parseStatement)
         <|> ((:[]) <$> parseStatement)

parseFn :: Parser TopLevelDeclaration
parseFn = Function <$> parseType <*> identifier <*> parenCommas ((,) <$> parseType <*> identifier) <*> ((const Nothing <$> tokenSymbol ";") <|> (Just <$> parseBlock))

parseTld :: Parser TopLevelDeclaration
parseTld = parseFn

parser :: Parser Program
parser = many parseTld <* eof

-- f `dot` g == \x y -> f (g x y)
dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.).(.)

parse :: SourceName -> String -> Program
parse = either (error . show) id `dot` runParser parser ()
