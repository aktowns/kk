module Node where

import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           Data.List        (intercalate)
import           Data.String.Conv (toS)
import           System.Console.Pretty

data KTemplateField = N Node
                    | T Type
                    deriving (Show)

data Type = TString
          | TNumber
          | THash Type
          | TList Type
          | TObjectRef Text 
          | TObject Text Type
          | TIntersection (Set (Text, Type))
          | TUnion (Set Type)
          deriving (Show, Ord, Eq)

green   = color Green
blue    = color Blue
magenta = color Magenta

ppType :: Type -> String
ppType TString            = green "String"
ppType TNumber            = green "Number"
ppType (THash x)          = blue "{" ++ ppType x ++ blue "}"
ppType (TList x)          = blue "[" ++ ppType x ++ blue "]"
ppType (TObjectRef n)     = show n
ppType (TObject n ty)     = magenta (toS n) ++ blue " { " ++ ppType ty ++ blue " }"
ppType (TIntersection xs) = intercalate ", " $ Set.toList $ Set.map (\(n,v) -> toS n ++ blue ": " ++ ppType v) xs
ppType (TUnion xs)        = intercalate (blue "|") $ Set.toList $ Set.map ppType xs

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