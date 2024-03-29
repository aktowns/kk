{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Control.Monad              (join)
import           Data.Bifunctor             (bimap)
import           Data.Maybe                 (catMaybes)
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
type PNode  = Node Position Check

trimnl = dropWhile (=='\n') . reverse . dropWhile (\x -> x =='\n' || x == ' ') . reverse
trims  = T.dropWhile (\x -> x == ' ' || x == '\n') . T.reverse . T.dropWhile (\x -> x == ' ' || x == '\n') . T.reverse

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

hereDoc :: Parser (StringType, Text)
hereDoc = do
  f <- (HereDoc <$ heredoc) <|> (HereDocStripped <$ heredocStrip)
  term <- T.pack <$> some upperChar
  txt <- T.pack . trimnl <$> manyTill L.charLiteral (string term)
  return (f term, txt)

interpolate :: Parser (Text, PNode)
interpolate = do
  (t, v) <- match $ do
    pos <- getPos
    _ <- symbol "%{"
    var <- kIdentifier
    _ <- symbol "}"
    return $ KVariable pos Untyped var
  return (trims t, v)

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

kDefine :: Parser PNode
kDefine = lexeme $ do
  pos  <- getPos
  _    <- symbol "%define"
  name <- kIdentifier
  args <- optional $ do
    _    <- lparen
    arglist <- kIdentifier `sepBy` comma
    _    <- rparen
    return arglist
  _    <- equal
  KDefine pos Untyped name args <$> kValue

kInclude :: Parser PNode
kInclude = lexeme $ do
  pos <- getPos
  _ <- symbol "%include"
  KInclude pos Untyped <$> stringLiteral

lineComment :: Parser PNode
lineComment = lexeme $ do
  pos <- getPos
  KComment pos Untyped <$> (string "#" *> takeWhileP (Just "character") (/= '\n'))

collectInterpolations :: Text -> [(Text, PNode)]
collectInterpolations txt =
  case results of
    (Left err) -> error (errorBundlePretty err)
    (Right xs) -> catMaybes xs
  where
    ignoreCharacter :: Parser (Maybe (Text, PNode))
    ignoreCharacter = Nothing <$ L.charLiteral

    interpolated :: Parser (Maybe (Text, PNode))
    interpolated    = Just <$> interpolate

    results = parse (many (interpolated <|> ignoreCharacter)) "interpolated string" txt

kNumber :: Parser PNode
kNumber = KNumber <$> getPos <*> pure Untyped <*> numberLiteral

kStringLiteral :: Parser PNode
kStringLiteral = do
  pos <- getPos
  str <- stringLiteral
  let inter = collectInterpolations str
  return $ KString pos Untyped Literal inter str

kHereDoc :: Parser PNode
kHereDoc = do
  pos <- getPos
  (term, txt) <- hereDoc
  let inter = collectInterpolations txt
  return $ KString pos Untyped term inter txt

kString :: Parser PNode
kString = kStringLiteral <|> kHereDoc

kBool :: Parser PNode
kBool = KBool <$> getPos <*> pure Untyped <*> ((True <$ symbol "true") <|> (False <$ symbol "false"))

kIdentifier :: Parser Text
kIdentifier = lexeme (T.pack <$> ((:) <$> letterChar <*> many alphaNumChar <?> "identifier"))

kSepIdentifier :: Parser Text
kSepIdentifier = do
  e <- kIdentifier
  c <- colon
  v <- kIdentifier
  return $ T.concat [e,c,v]

kCall :: Parser PNode
kCall = lexeme $ do
  pos <- getPos
  _ <- percent
  name <- try kSepIdentifier <|> kIdentifier
  args <- optional $ do
    _ <- lparen
    arglist <- kValue `sepBy` comma
    _ <- rparen
    return arglist
  return $ case args of
    Just a  -> KCall pos Untyped name a
    Nothing -> KVariable pos Untyped name

kTopLevel :: Parser [PNode]
kTopLevel = lexeme $ many (kDefine <|> kInclude <|> kTemplate <|> kObject <|> kCall <|> lineComment) <* eof

kValue :: Parser PNode
kValue  = lexeme $ kNumber <|> kString <|> kBool <|> kHash <|> kList <|> kObject <|> kCall <|> lineComment

kHashItem :: Parser (Text, PNode)
kHashItem = do
  key <- kIdentifier
  _ <- colon
  value <- kValue
  return (key, value)

kHash :: Parser PNode
kHash = do
  pos <- getPos
  _ <- lbrack
  items <- kHashItem `sepBy` comma
  _ <- rbrack
  return $ KHash pos Untyped items

kList :: Parser PNode
kList = do
  pos <- getPos
  _ <- lblock
  items <- kValue `sepBy` comma
  _ <- rblock
  return $ KList pos Untyped items

kKVPair :: Parser a -> Parser (Text, PNode)
kKVPair p = do
  name <- kIdentifier
  _ <- p
  body <- kValue
  return (name, body)

kObjectItem :: Parser (Text, PNode)
kObjectItem = kKVPair equal

kObject :: Parser PNode
kObject = lexeme $ do
  pos <- getPos
  name <- kIdentifier
  _ <- lbrack
  body <- many kObjectItem
  _ <- rbrack
  return $ KObject pos Untyped name body

kTemplate :: Parser PNode
kTemplate = lexeme $ do
  pos <- getPos
  _ <- symbol "%template"
  name <- kIdentifier
  _ <- lbrack
  body <- many kTemplateItem
  _ <- rbrack
  return $ KTemplate pos Untyped name body

kTemplateItem :: Parser (Text, KTemplateField Position Check)
kTemplateItem = lexeme $ try kTypeTemplateField <|> kNodeTemplateField

kNodeTemplateField :: Parser (Text, KTemplateField Position Check)
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

kTypeTemplateField :: Parser (Text, KTemplateField Position Check)
kTypeTemplateField = lexeme $ do
  name <- kIdentifier
  _ <- colon
  body <- kAllType
  return (name, T body)

kParse :: FilePath -> IO [PNode]
kParse fn = do
  source <- T.readFile fn
  case parse (sc *> kTopLevel) fn source of
    Left bundle -> error (errorBundlePretty bundle)
    Right xs    -> return xs

kParseAll :: FilePath -> IO [PNode]
kParseAll fn = collectImports =<< kParse fn

collectImports :: [PNode] -> IO [PNode]
collectImports xs = join <$> traverse inner xs
  where
    inner :: PNode -> IO [PNode]
    inner (KInclude _ _ fn) = kParseAll $ toS fn
    inner x                 = return [x]
