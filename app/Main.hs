{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib

import System.Console.CmdArgs

data OutFormat = YAML | JSON deriving (Show, Data, Typeable)

instance Default OutFormat where
  def = YAML

data KK = KK { inFile    :: FilePath 
             , outFile   :: Maybe FilePath
             , outFormat :: OutFormat
             } deriving (Show, Data, Typeable)

kk = KK { inFile    = def &= argPos 0 &= typ "IN FILE"
        , outFile   = def &= typ "OUT FILE"  &= help "File to output, defaults to stdout"
        , outFormat = def &= typ "YAML|JSON" &= help "Format of the output file"
        } &= 
        verbosity &=
        help "to yaml or json configuration compiler" &=
        summary "kk v0.0.0, (C) Ashley Towns" &=
        details ["compiles your kk file to either json or yaml"]

mode = cmdArgsMode kk

main :: IO ()
main = do 
  args <- cmdArgsRun mode
  output <- case outFormat args of
    YAML -> kCompile $ inFile args
    JSON -> kCompileJson $ inFile args
  case outFile args of
    Just fn -> writeFile fn output
    Nothing -> putStrLn output