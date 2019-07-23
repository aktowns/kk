{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Data.Foldable                             (traverse_)
import Data.Text                                 (Text)
import Data.Text.Prettyprint.Doc                 hiding (list)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Symbols.Ascii

import Node (Node (..), StringType (..))

hash = encloseSep (annotate "keyword" lbrace <> space) (space <> annotate "keyword" rbrace) comma
list = encloseSep (annotate "keyword" lbracket <> space) (space <> annotate "keyword" rbracket) (comma <> space)
args x = hsep $ punctuate comma x

kv c (k, v) = pretty k <> c <+> nodeText v

printNodeText :: Node a b -> IO ()
printNodeText node = putDoc $ reAnnotate term (nodeText node)
  where
    term "literal"  = color Green
    term "keyword"  = color Blue
    term "name"     = color Magenta
    term "variable" = color Yellow
    term "comment"  = color Red
    term "heredoc"  = color Red

printNodeTexts :: [Node a b] -> IO ()
printNodeTexts = traverse_ printNodeText

deindent d = column (\x -> hang (-x) d)

nodeText :: Node a b -> Doc Text
nodeText (KString _ _ Literal _ s)    = annotate "literal" $ dquotes $ pretty s
nodeText (KString _ _ (HereDoc term) _ s)    =
  annotate "heredoc" $ "<<-" <> align (pretty term <> deindent (line <> pretty s)) <> line <> pretty term
nodeText (KString _ _ (HereDocStripped term) _ s)    =
  annotate "heredoc" $ "<<~" <> pretty term <> line <> pretty s <> line <> pretty term
nodeText (KNumber _ _ n)    = annotate "literal" $ pretty n
nodeText (KBool   _ _ b)    = annotate "literal" $ pretty b
nodeText (KList   _ _ xs)   = list $ map nodeText xs
nodeText (KHash   _ _ xs)   = hash $ map (kv colon) xs
nodeText (KObject _ _ n xs) =
  annotate "name" (pretty n)
    <+> align (lbrace
    <+> nest 2 (vsep (map (kv (space <> equals)) xs))
     <> (line <> rbrace))
nodeText (KTemplate _ _ n x) = annotate "keyword" "%template" <+> pretty n
nodeText (KVariable _ _ s)   = annotate "variable" ("$" <> pretty s)
nodeText (KDefine _ _ n (Just a) b) =
  annotate "keyword" "%define" <+> pretty n <> "(" <> args (map pretty a) <> ")" <+> "=" <> line
    <> indent 2 (nodeText b) <> line <> line
nodeText (KDefine _ _ n Nothing b) =
  annotate "keyword" "%define" <+> pretty n <+> "=" <+> nodeText b <> line <> line
nodeText (KCall _ _ n a)     =
  annotate "keyword" ("%" <> pretty n) <> "(" <> args (map nodeText a) <> ")"
nodeText (KInclude _ _ s)    =
  annotate "keyword" "%include" <+> annotate "literal" (dquotes $ pretty s) <> line <> line
nodeText (KComment _ _ s)    = annotate "comment" ("#" <> pretty s) <> line

nodeTexts :: [Node a b] -> Doc Text
nodeTexts xs = vsep $ map nodeText xs
