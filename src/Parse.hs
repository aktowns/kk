{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Control.Monad              (join)
import           Data.Bifunctor             (bimap)
import qualified Data.Set                   as Set
import           Data.String.Conv           (toS)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

import Node

import Debug.Trace

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

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

trimnl = dropWhile (=='\n') . reverse . dropWhile (\x -> x =='\n' || x == ' ') . reverse

hereDoc :: Parser (StringType, Text)
hereDoc = do
  f <- (HereDoc <$ heredoc) <|> (HereDocStripped <$ heredocStrip)
  term <- T.pack <$> some upperChar
  txt <- T.pack . trimnl <$> manyTill L.charLiteral (string term)
  return (f term, txt)

numberLiteral :: Parser Float
numberLiteral = lexeme (try L.float <|> (fromIntegral <$> L.decimal))

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

heredoc :: Parser Text
heredoc = symbol "<<-"

heredocStrip :: Parser Text
heredocStrip = symbol "<<~"

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

lineComment :: Parser Node
lineComment = lexeme $ do
  pos <- getPos
  KComment pos Untyped <$> (string "#" *> takeWhileP (Just "character") (/= '\n'))

kNumber :: Parser Node
kNumber = KNumber <$> getPos <*> pure Untyped <*> numberLiteral

kStringLiteral :: Parser Node
kStringLiteral = KString <$> getPos <*> pure Untyped <*> pure Literal <*> pure [] <*> stringLiteral

kHereDoc :: Parser Node
kHereDoc = do
  pos <- getPos
  (term, txt) <- hereDoc
  return $ KString pos Untyped term [] txt

kString :: Parser Node
kString = kStringLiteral <|> kHereDoc

kBool :: Parser Node
kBool = KBool <$> getPos <*> pure Untyped <*> ((True <$ symbol "true") <|> (False <$ symbol "false"))

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
kTopLevel = lexeme $ many (kDefine <|> kInclude <|> kTemplate <|> kObject <|> kCall <|> lineComment) <* eof

kValue :: Parser Node
kValue  = lexeme $ kNumber <|> kString <|> kBool <|> kHash <|> kList <|> kObject <|> kVariable <|> kCall <|> lineComment

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

kKVPair :: Parser a -> Parser (Text, Node)
kKVPair p = do
  name <- kIdentifier
  _ <- p
  body <- kValue
  return (name, body)

kObjectItem :: Parser (Text, Node)
kObjectItem = kKVPair equal

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
kNodeTemplateField = lexeme $ bimap id N <$> kKVPair equal

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
    Right xs    -> return xs

kParseAll :: FilePath -> IO [Node]
kParseAll fn = collectImports =<< kParse fn

collectImports :: [Node] -> IO [Node]
collectImports xs = join <$> traverse inner xs
  where
    inner :: Node -> IO [Node]
    inner (KInclude _ _ fn) = kParseAll $ toS fn
    inner x                 = return [x]
