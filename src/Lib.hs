module Lib where

import Node
import Eval
import Parse
import Compile

kRead :: FilePath -> IO [Node]
kRead fp = do 
    ast <- kParse fp
    return $ evalKK emptyCtx (reduceAll ast)

kCompile :: FilePath -> IO String
kCompile fp = toYaml <$> kRead fp

kCompileJson :: FilePath -> IO String
kCompileJson fp = toJson <$> kRead fp