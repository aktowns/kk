module Node where

import           Data.List        (intercalate)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.String.Conv (toS)
import           Data.Text        (Text)

data KTemplateField a b = N (Node a b)
                        | T Type
                        deriving (Show, Eq)

data Position = Position { line   :: Int
                         , column :: Int
                         , file   :: String
                         } deriving (Show, Eq)

data Type = TString
          | TNumber
          | TBool
          | THash Type
          | TList Type
          | TObjectRef Text
          | TObject Text Type
          | TIntersection (Set (Text, Type))
          | TIntersectionRef (Set Type)
          | TUnion (Set Type)
          deriving (Show, Ord, Eq)

ppType :: Type -> String
ppType TString            = "String"
ppType TNumber            = "Number"
ppType (THash x)          = "{" ++ ppType x ++ "}"
ppType (TList x)          = "[" ++ ppType x ++ "]"
ppType (TObjectRef n)     = show n
ppType (TObject n ty)     = toS n ++ " { " ++ ppType ty ++ " }"
ppType (TIntersection xs) = intercalate ", " $ Set.toList $ Set.map (\(n,v) -> toS n ++ ": " ++ ppType v) xs
ppType (TUnion xs)        = intercalate "|" $ Set.toList $ Set.map ppType xs

data Check = Typed Type 
           | Untyped 
           deriving (Show, Eq)

data StringType = Literal
                | HereDoc Text
                | HereDocStripped Text
                deriving (Show, Eq)

data Node a b = KObject   a b Text [(Text, Node a b)]
              | KList     a b [Node a b]
              | KHash     a b [(Text, Node a b)]
              | KString   a b StringType [(Text, Node a b)] Text
              | KNumber   a b Float
              | KBool     a b Bool
              | KComment  a b Text
              | KDefine   a b Text (Maybe [Text]) (Node a b)
              | KCall     a b Text [Node a b]
              | KInclude  a b Text
              | KVariable a b Text
              | KTemplate a b Text [(Text, KTemplateField a b)]
              deriving (Show, Eq)

ppCheck :: Check -> String
ppCheck (Typed ty) = ppType ty
ppCheck Untyped    = "Untyped"
