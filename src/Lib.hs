module Lib where

import Compile
import Eval
import Node
import Parse
import Pretty

kRead :: FilePath -> IO [Node Position Check]
kRead fp = do
  ast <- kParseAll fp
  return $ evalKK emptyCtx (reduceAll ast)

kCompile :: FilePath -> IO String
kCompile fp = toYaml <$> kRead fp

kCompileJson :: FilePath -> IO String
kCompileJson fp = toJson <$> kRead fp

kFormat :: FilePath -> IO ()
kFormat fp = printNodeTexts =<< kParse fp
