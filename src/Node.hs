module Node where

import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           Data.List        (intercalate)
import           Data.String.Conv (toS)

data KTemplateField = N Node
                    | T Type
                    deriving (Show, Eq)

data Position = Position { line :: Int, column :: Int, file :: String } deriving (Show, Eq)

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

data Check = Typed Type | Untyped deriving (Show, Eq)

data Node = KObject   Position Check Text ![(Text, Node)]
          | KList     Position Check ![Node]
          | KHash     Position Check ![(Text, Node)]
          | KString   Position Check Text
          | KNumber   Position Check Float
          | KBool     Position Check Bool
          | KComment  Position Check Text
          | KDefine   Position Check Text [Text] !Node
          | KCall     Position Check Text ![Node]
          | KInclude  Position Check Text
          | KVariable Position Check Text
          | KTemplate Position Check Text ![(Text, KTemplateField)]
          deriving (Show, Eq)

ppCheck :: Check -> String
ppCheck (Typed ty) = ppType ty
ppCheck Untyped    = "Untyped"
