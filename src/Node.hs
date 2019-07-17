module Node where

import           Data.Text    (Text)

data KTemplateField = N Node
                    | T Type
                    deriving (Show)

data Type = TString
          | TNumber
          | THash Type
          | TArray Type
          | TObject Text
          deriving (Show)

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