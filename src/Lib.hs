{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, BangPatterns #-}
module Lib where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Text.Megaparsec.Char.Lexer as L
import           Debug.Trace (trace)
import           Text.Megaparsec.Debug

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Aeson as A

data KTemplateField = N Node | T Type deriving (Show)

data Type = TString | TNumber | THash Type | TArray Type | TObject Text deriving (Show)

data Node = KObject Text ![(Text, Node)]
          | KList ![Node]
          | KHash ![(Text, Node)]
          | KString Text
          | KNumber Float
          | KDefine Text [Text] !Node
          | KCall Text ![Node]
          | KInclude Text
          | KVariable Text
          | KTemplate Text ![(Text, KTemplateField)]
          deriving (Show)

data Ctx = Ctx { env :: HM.HashMap Text ([Text], Node)
               , var :: HM.HashMap Text Node 
               } deriving (Show)

emptyCtx :: Ctx
emptyCtx = Ctx { env = HM.empty, var = HM.empty }

envInsert :: Ctx -> Text -> [Text] -> Node -> Ctx
envInsert Ctx{..} n a b = Ctx { env = HM.insert n (a, b) env, var = var }

envGet :: Ctx -> Text -> ([Text], Node)
envGet Ctx{..} n = fromMaybe (error $ show n ++ " is not defined") (HM.lookup n env)
        
varGet :: Ctx -> Text -> Node
varGet Ctx{..} n = fromMaybe (error $ show n ++ " is not defined") (HM.lookup n var)

envApply :: Ctx -> Text -> [Node] -> Maybe Node
envApply ctx n a = 
    let (a', n') = envGet ctx n
        e        = ctx { var = HM.fromList $ zip a' a }
    in snd $ reduce e n'
    
evTup :: [(Text, Node)] -> [(Text, A.Value)]
evTup = map (\(x, y) -> (x, toValue y))

toValue :: Node -> A.Value
toValue (KObject _ body) = A.Object $ HM.fromList $ evTup body
toValue (KHash body)     = A.Object $ HM.fromList $ evTup body
toValue (KList xs)       = A.Array $ V.fromList $ map toValue xs
toValue (KString s)      = A.String s

type Parser = Parsec Void Text

mapCtx :: Ctx -> (Ctx -> a -> (Ctx, b)) -> [a] -> [b]
mapCtx e f = inner e
    where 
        inner e' []    = []
        inner e' (h:t) = let (e'', h') = f e' h in h' : inner e'' t

tupFlip :: (a, Maybe b) -> Maybe (a, b)
tupFlip (a, Just b)  = Just (a, b)
tupFlip (a, Nothing) = Nothing

reduceFn :: Ctx -> (Text, Node) -> (Ctx, Maybe (Text, Node))
reduceFn e (t, x) = let (e', x') = reduce e x in (e', tupFlip (t, x'))

reduce :: Ctx -> Node -> (Ctx, Maybe Node)
reduce e x@(KObject n body) = (e, Just $ KObject n (catMaybes $ mapCtx e reduceFn body))
reduce e x@(KList xs)       = (e, Just $ KList $ catMaybes $ mapCtx e reduce xs)
reduce e x@(KDefine n a b)  = (envInsert e n a b, Nothing)
reduce e x@(KCall n a)      = (e, envApply e n a)
reduce e x@(KVariable n)    = (e, Just $ varGet e n)
reduce e k                  = (e, Just k)

reduceAll :: Ctx -> [Node] -> [Node]
reduceAll e x = catMaybes $ mapCtx e reduce x
 
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

kParse fn = do
    source <- T.readFile fn
    case parse (sc *> kTopLevel) fn source of
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            return Nothing
        Right xs -> return $ Just xs
