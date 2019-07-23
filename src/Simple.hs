{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple where

import Data.Text (Text)

import qualified Node as N

empty = N.Position 0 0 ""

type Node = N.Node () ()
pattern KObject a b = N.KObject () () (a :: Text) (b :: [(Text, Node)])
pattern KList a = N.KList () () (a :: [Node])
pattern KHash a = N.KHash () () (a :: [(Text, Node)])
pattern KString a b c = N.KString () () (a :: N.StringType) (b :: [(Text, Node)]) (c :: Text)
pattern KNumber a = N.KNumber () () (a :: Float)
pattern KBool a = N.KBool () () (a :: Bool)
pattern KComment a = N.KComment () () (a :: Text)
pattern KDefine a b c = N.KDefine () () (a :: Text) (b :: (Maybe [Text])) (c :: Node)
pattern KCall a b = N.KCall () () (a :: Text) (b :: [Node])
pattern KInclude a = N.KInclude () () (a :: Text)
pattern KVariable a = N.KVariable () () (a :: Text)
pattern KTemplate a b = N.KTemplate () () (a :: Text) (b :: [(Text, N.KTemplateField () ())])