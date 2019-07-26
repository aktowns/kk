module Lib where

import Data.Text (Text)

import Compile
import Eval
import Node
import Parse
import Pretty

kRead :: FilePath -> [(Text, Text)] -> IO [Node Position Check]
kRead fp env = do
  ast <- kParseAll fp
  return $ evalKK (ctxFromKV env) (reduceAll ast)

kCompile :: FilePath -> [(Text, Text)] -> IO String
kCompile fp env = toYaml <$> kRead fp env

kCompileJson :: FilePath -> [(Text, Text)] -> IO String
kCompileJson fp env = toJson <$> kRead fp env

kFormat :: FilePath -> IO ()
kFormat fp = printNodeTexts =<< kParse fp
