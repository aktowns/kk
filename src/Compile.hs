module Compile where

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import           Data.List           (intercalate)
import           Data.String.Conv    (toS)
import           Data.Text           (Text)
import qualified Data.Vector         as V
import qualified Data.Yaml           as Y

import Debug.Trace (trace)

import Node

evTup :: [(Text, Node)] -> [(Text, A.Value)]
evTup = map (\(x, y) -> (x, toValue y))

toValue :: Node -> A.Value
toValue (KObject _ _ _ body) = A.Object $ HM.fromList $ evTup body
toValue (KHash _ _ body)     = A.Object $ HM.fromList $ evTup body
toValue (KList _ _ xs)       = A.Array $ V.fromList $ map toValue xs
toValue (KString _ _ _ i s)  = A.String s

toYaml :: [Node] -> String
toYaml xs = intercalate "\n---\n" $ map (toS . Y.encode . toValue) xs

toJson :: [Node] -> String
toJson xs = intercalate "\n---\n" $ map (toS . A.encode . toValue) xs
