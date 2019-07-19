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
import qualified Data.Set as Set
import           Control.Monad (join)
import           Data.String.Conv (toS)

import           Node

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#{" "#}")

lexeme = L.lexeme sc

symbol = L.symbol sc

getPos :: Parser Position
getPos = do
    pos <- getSourcePos
    return $ Position { line = unPos $ sourceLine pos
                      , column = unPos $ sourceColumn pos
                      , file = sourceName pos 
                      }

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

pipe :: Parser Text
pipe = symbol "|"

ampersand :: Parser Text
ampersand = symbol "&"

kDefine :: Parser Node
kDefine = lexeme $ do
    pos  <- getPos
    _    <- symbol "%define"
    name <- kIdentifier
    _    <- lparen
    args <- kIdentifier `sepBy` comma
    _    <- rparen
    _    <- equal
    KDefine pos Untyped name args <$> kValue

kInclude :: Parser Node
kInclude = lexeme $ do
    pos <- getPos
    _ <- symbol "%include"
    KInclude pos Untyped <$> stringLiteral

kNumber :: Parser Node
kNumber = KNumber <$> getPos <*> pure Untyped <*> lexeme (L.float <|> (fromIntegral <$> L.decimal))

kString :: Parser Node
kString = KString <$> getPos <*> pure Untyped <*> stringLiteral

kBool :: Parser Node
kBool = KBool <$> getPos <*> pure Untyped <*> ((const True <$> symbol "true") <|> (const False <$> symbol "false"))

kIdentifier :: Parser Text
kIdentifier = lexeme (T.pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "identifier"))

kVariable :: Parser Node
kVariable = KVariable <$> getPos <*> pure Untyped <*> (dollar *> kIdentifier) <?> "variable"

kCall :: Parser Node
kCall = lexeme $ do
    pos <- getPos
    _ <- percent
    name <- kIdentifier
    _ <- lparen 
    args <- kValue `sepBy` comma
    _ <- rparen
    return $ KCall pos Untyped name args

kTopLevel :: Parser [Node]
kTopLevel = lexeme $ many (kDefine <|> kInclude <|> kTemplate <|> kObject <|> kCall) <* eof

kValue :: Parser Node
kValue  = lexeme $ kNumber <|> kString <|> kBool <|> kHash <|> kList <|> kObject <|> kVariable <|> kCall

kHashItem :: Parser (Text, Node)
kHashItem = do
    key <- kIdentifier
    _ <- colon
    value <- kValue
    return (key, value)

kHash :: Parser Node
kHash = do 
    pos <- getPos
    _ <- lbrack
    items <- kHashItem `sepBy` comma
    _ <- rbrack
    return $ KHash pos Untyped items

kList :: Parser Node
kList = do
    pos <- getPos
    _ <- lblock
    items <- kValue `sepBy` comma
    _ <- rblock
    return $ KList pos Untyped items

kObjectItem :: Parser (Text, Node)
kObjectItem = do
    name <- kIdentifier
    _ <- equal
    body <- kValue
    return (name, body)
    
kObject :: Parser Node
kObject = lexeme $ do
    pos <- getPos
    name <- kIdentifier
    _ <- lbrack
    body <- many kObjectItem
    _ <- rbrack
    return $ KObject pos Untyped name body

kTemplate :: Parser Node
kTemplate = lexeme $ do
    pos <- getPos
    _ <- symbol "%template"
    name <- kIdentifier
    _ <- lbrack
    body <- many kTemplateItem
    _ <- rbrack
    return $ KTemplate pos Untyped name body

kTemplateItem :: Parser (Text, KTemplateField)
kTemplateItem = lexeme $ try kTypeTemplateField <|> kNodeTemplateField

kNodeTemplateField :: Parser (Text, KTemplateField)
kNodeTemplateField = lexeme $ do
    name <- kIdentifier
    _ <- equal
    body <- kValue
    return (name, N body)

kUnionType :: Parser Type
kUnionType = lexeme $ do
    tys <- kType `sepBy1` pipe
    return $ TUnion $ Set.fromList tys

kIntersectionType :: Parser Type
kIntersectionType = lexeme $ do
    tys <- kType `sepBy1` ampersand
    return $ TIntersectionRef $ Set.fromList tys

kListType :: Parser Type
kListType = lexeme $ do
    _ <- lblock
    ty <- kAllType
    _ <- rblock
    return $ TList ty

kHashType :: Parser Type
kHashType = lexeme $ do
    _ <- lbrack
    ty <- kAllType
    _ <- rbrack
    return $ THash ty

kStringType :: Parser Type
kStringType = TString <$ symbol "String"

kNumberType :: Parser Type
kNumberType = TNumber <$ symbol "Number"

kBoolType :: Parser Type
kBoolType = TBool <$ symbol "Bool"

kAllType :: Parser Type
kAllType  = kStringType
        <|> kNumberType
        <|> kBoolType
        <|> kListType
        <|> kHashType
        <|> kUnionType
        <|> kIntersectionType
        <|> (TObjectRef <$> kIdentifier)

kType :: Parser Type
kType  = kStringType
     <|> kNumberType
     <|> kBoolType
     <|> kListType
     <|> kHashType
     <|> (TObjectRef <$> kIdentifier)

kTypeTemplateField :: Parser (Text, KTemplateField)
kTypeTemplateField = lexeme $ do
    name <- kIdentifier
    _ <- colon
    body <- kAllType
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
    inner (KInclude _ _ fn) = kParse $ toS fn
    inner x = return [x]