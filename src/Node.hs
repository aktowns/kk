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

data Position = Position { line :: Int, column :: Int, file :: String } deriving (Show)

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

green   = color Green
blue    = color Blue
magenta = color Magenta
red     = color Red

ppType :: Type -> String
ppType TString            = green "String"
ppType TNumber            = green "Number"
ppType (THash x)          = blue "{" ++ ppType x ++ blue "}"
ppType (TList x)          = blue "[" ++ ppType x ++ blue "]"
ppType (TObjectRef n)     = show n
ppType (TObject n ty)     = magenta (toS n) ++ blue " { " ++ ppType ty ++ blue " }"
ppType (TIntersection xs) = intercalate ", " $ Set.toList $ Set.map (\(n,v) -> toS n ++ blue ": " ++ ppType v) xs
ppType (TUnion xs)        = intercalate (blue "|") $ Set.toList $ Set.map ppType xs

data Check = Typed Type | Untyped deriving (Show)

data Node = KObject   Position Check Text ![(Text, Node)]
          | KList     Position Check ![Node]
          | KHash     Position Check ![(Text, Node)]
          | KString   Position Check Text
          | KNumber   Position Check Float
          | KBool     Position Check Bool
          | KDefine   Position Check Text [Text] !Node
          | KCall     Position Check Text ![Node]
          | KInclude  Position Check Text
          | KVariable Position Check Text
          | KTemplate Position Check Text ![(Text, KTemplateField)]
          deriving (Show)

ppCheck :: Check -> String
ppCheck (Typed ty) = blue $ ppType ty
ppCheck Untyped    = red "Untyped"

ppNode :: Node -> String
ppNode = ppNodeIn 0

ci n s = map (const ' ') [0..(n*4)] ++ s

ppNodeIn :: Int -> Node -> String
ppNodeIn ind (KString _ t s)    = ci ind $ green $ toS s ++ " :: " ++ ppCheck t
ppNodeIn ind (KNumber _ t s)    = ci ind $ green $ show s ++ " :: " ++ ppCheck t
ppNodeIn ind (KBool _ t s)      = ci ind $ green $ show s ++ " :: " ++ ppCheck t
ppNodeIn ind (KList _ t xs)     = ci ind $ green $ "[" ++ (intercalate ", " $ map ppNode xs) ++ "] :: " ++ ppCheck t 
ppNodeIn ind (KHash _ t xs)     = ci ind $ green $ "{" ++ (intercalate ", " $ map (\(x, y) -> toS x ++ ": " ++ ppNode y) xs) ++ "} :: " ++ ppCheck t 
ppNodeIn ind (KObject _ t n xs) = 
    ci ind $ magenta (toS n) ++ " {\n" ++ ci ind $ (intercalate ",\n" $ map (\(x, y) -> ci (ind+1) $ toS x ++ " = " ++ ppNodeIn ind y) xs) ++ " } :: " ++ ppCheck t
