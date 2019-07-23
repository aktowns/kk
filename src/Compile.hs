module Compile where

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import           Data.List           (intercalate)
import           Data.String.Conv    (toS)
import           Data.Text           (Text)
import qualified Data.Vector         as V
import qualified Data.Yaml           as Y
import Data.Scientific (fromFloatDigits)

import Debug.Trace (trace)

import Node

evTup :: (Show a, Show b) => [(Text, Node a b)] -> [(Text, A.Value)]
evTup = map (\(x, y) -> (x, toValue y))

toValue :: (Show a, Show b) => Node a b -> A.Value
toValue (KObject _ _ _ body) = A.Object $ HM.fromList $ evTup body
toValue (KHash _ _ body)     = A.Object $ HM.fromList $ evTup body
toValue (KList _ _ xs)       = A.Array $ V.fromList $ map toValue xs
toValue (KString _ _ _ i s)  = A.String s
toValue (KNumber _ _ n)      = A.Number $ fromFloatDigits n
toValue x                    = error $ "unexpected compilation error: " ++ show x

toYaml :: (Show a, Show b) => [Node a b] -> String
toYaml xs = intercalate "\n---\n" $ map (toS . Y.encode . toValue) xs

toJson :: (Show a, Show b) => [Node a b] -> String
toJson xs = intercalate "\n---\n" $ map (toS . A.encode . toValue) xs
