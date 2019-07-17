{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Void
import           Control.Monad (join)
import           Data.String.Conv (toS)

import Node

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#{" "#}")

lexeme = L.lexeme sc

symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

lbrack :: Parser Text
lbrack  = symbol "{"

rbrack :: Parser Text
rbrack  = symbol "}"

lblock :: Parser Text
lblock  = symbol "["

rblock :: Parser Text
rblock  = symbol "]"

lparen :: Parser Text
lparen  = symbol "("

rparen :: Parser Text
rparen  = symbol ")"

semi :: Parser Text
semi    = symbol ";"

colon :: Parser Text
colon   = symbol ":"

comma :: Parser Text
comma   = symbol ","

equal :: Parser Text
equal   = symbol "="

percent :: Parser Text
percent = symbol "%"

dollar :: Parser Text
dollar  = symbol "$"

kDefine :: Parser Node
kDefine = lexeme $ do
    _    <- symbol "%define"
    name <- kIdentifier
    _    <- lparen
    args <- kIdentifier `sepBy` comma
    _    <- rparen
    _    <- equal
    KDefine name args <$> kValue

kInclude :: Parser Node
kInclude = lexeme $ do
    _ <- symbol "%include"
    KInclude <$> stringLiteral

kNumber :: Parser Node
kNumber = KNumber <$> lexeme (L.float <|> (fromIntegral <$> L.decimal))

kString :: Parser Node
kString = KString <$> stringLiteral

kIdentifier :: Parser Text
kIdentifier = lexeme (T.pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "identifier"))

kVariable :: Parser Node
kVariable = KVariable <$> (dollar *> kIdentifier) <?> "variable"

kCall :: Parser Node
kCall = lexeme $ do
    _ <- percent
    name <- kIdentifier
    _ <- lparen 
    args <- kValue `sepBy` comma
    _ <- rparen
    return $ KCall name args

kTopLevel :: Parser [Node]
kTopLevel = lexeme $ many (kDefine <|> kInclude <|> kTemplate <|> kObject <|> kCall) <* eof

kValue :: Parser Node
kValue  = lexeme $ kNumber <|> kString <|> kHash <|> kList <|> kObject <|> kVariable <|> kCall

kHashItem :: Parser (Text, Node)
kHashItem = do
    key <- kIdentifier
    _ <- colon
    value <- kValue
    return (key, value)

kHash :: Parser Node
kHash = do 
    _ <- lbrack
    items <- kHashItem `sepBy` comma
    _ <- rbrack
    return $ KHash items

kList :: Parser Node
kList = do
    _ <- lblock
    items <- kValue `sepBy` comma
    _ <- rblock
    return $ KList items

kObjectItem :: Parser (Text, Node)
kObjectItem = do
    name <- kIdentifier
    _ <- equal
    body <- kValue
    return (name, body)
    
kObject :: Parser Node
kObject = lexeme $ do
    name <- kIdentifier
    _ <- lbrack
    body <- many kObjectItem
    _ <- rbrack
    return $ KObject name body

kTemplate :: Parser Node
kTemplate = lexeme $ do
    _ <- symbol "%template"
    name <- kIdentifier
    _ <- lbrack
    body <- many kTemplateItem
    _ <- rbrack
    return $ KTemplate name body

kTemplateItem :: Parser (Text, KTemplateField)
kTemplateItem = lexeme $ try kTypeTemplateField <|> kNodeTemplateField

kNodeTemplateField :: Parser (Text, KTemplateField)
kNodeTemplateField = lexeme $ do
    name <- kIdentifier
    _ <- equal
    body <- kValue
    return (name, N body)

kArrayType :: Parser Type
kArrayType = lexeme $ do
    _ <- lblock
    ty <- kType
    _ <- rblock
    return $ TArray ty

kHashType :: Parser Type
kHashType = lexeme $ do
    _ <- lbrack
    ty <- kType
    _ <- rbrack
    return $ THash ty

kType :: Parser Type
kType  = (TString <$ symbol "String")
     <|> (TNumber <$ symbol "Number")
     <|> kArrayType
     <|> kHashType
     <|> (TObject <$> kIdentifier)

kTypeTemplateField :: Parser (Text, KTemplateField)
kTypeTemplateField = lexeme $ do
    name <- kIdentifier
    _ <- colon
    body <- kType
    return (name, T body)


kParse :: FilePath -> IO [Node]
kParse fn = do
    source <- T.readFile fn
    case parse (sc *> kTopLevel) fn source of
        Left bundle -> error (errorBundlePretty bundle)
        Right xs -> collectImports xs

collectImports :: [Node] -> IO [Node]
collectImports xs = join <$> traverse inner xs
  where
    inner :: Node -> IO [Node]
    inner (KInclude fn) = kParse $ toS fn
    inner x = return [x]