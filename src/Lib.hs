{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Lib where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (catMaybes)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Aeson as A

data Node = KObject Text [(Text, Node)]
          | KList [Node]
          | KHash [(Text, Node)]
          | KString Text
          | KNumber Float
          | KDefine Text [Text] Node
          | KCall Text [Node]
          | KInclude Text
          | KVariable Text
          deriving (Show)

data Ctx = Ctx { env :: HM.HashMap Text ([Text], Node) }

emptyCtx :: Ctx
emptyCtx = Ctx { env = HM.empty }

envInsert :: Ctx -> Text -> [Text] -> Node -> Ctx
envInsert Ctx{..} n a b = Ctx { env = HM.insert n (a, b) env }

evTup :: [(Text, Node)] -> [(Text, A.Value)]
evTup = map (\(x, y) -> (x, toValue y))

toValue :: Node -> A.Value
toValue (KObject _ body) = A.Object $ HM.fromList $ evTup body
toValue (KHash body)     = A.Object $ HM.fromList $ evTup body
toValue (KList xs)       = A.Array $ V.fromList $ map toValue xs
toValue (KString s)      = A.String s

type Parser = Parsec Void Text

mapCtx :: Ctx -> (Ctx -> a -> (Ctx, a)) -> [a] -> [a]
mapCtx e f xs = inner e xs
    where 
        inner e' []    = []
        inner e' (h:t) = let (e'', h') = f e' h in h' : (inner e'' t)

reduceFn :: Ctx -> (Text, Node) -> (Ctx, (Text, Node))
reduceFn e x = (e, x)

reduce :: Ctx -> Node -> (Ctx, Node)
reduce e (KObject n body) = (e, KObject n (mapCtx e reduceFn body))
reduce e x@(KDefine n a b)  = (envInsert e n a b, x)
 
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#{" "#}")

lexeme = L.lexeme sc

symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

lbrack  = symbol "{"
rbrack  = symbol "}"
lblock  = symbol "["
rblock  = symbol "]"
lparen  = symbol "("
rparen  = symbol ")"
semi    = symbol ";"
colon   = symbol ":"
comma   = symbol ","
equal   = symbol "="
percent = symbol "%"
dollar  = symbol "$"

kDefine :: Parser Node
kDefine = lexeme $ do
    _    <- symbol "%define"
    name <- kIdentifier
    args <- kIdentifier `sepBy` space1
    _    <- equal
    body <- kValue
    return $ KDefine name args body

kInclude :: Parser Node
kInclude = lexeme $ do
    _ <- symbol "%include"
    file <- stringLiteral
    return $ KInclude file

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
    args <- many kValue
    _ <- rparen
    return $ KCall name args

kTopLevel :: Parser [Node]
kTopLevel = lexeme $ many (kDefine <|> kInclude <|> kObject <|> kCall) <* eof

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

kParse source = 
    case parse (sc *> kTopLevel) "" source of
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            return Nothing
        Right xs -> return $ Just xs

