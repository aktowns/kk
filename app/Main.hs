{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import Lib

import System.Console.CmdArgs

data OutFormat = YAML | JSON deriving (Show, Data, Typeable, Eq)

instance Default OutFormat where
  def = YAML

data KK = Compile { inFile  :: FilePath
                  , outFile :: Maybe FilePath
                  , format  :: OutFormat
                  }
        | Format { inFile  :: FilePath
                 , outFile :: Maybe FilePath
                 , colour  :: Bool
                 }
        | Lint   { inFile  :: FilePath
                 , inPlace :: Bool
                 }
        | LSP    {}
        deriving (Show, Data, Typeable, Eq)

fin x = x &= argPos 0 &= typ "FILE"
fout x = x &= typ "FILE"  &= help "File to output, defaults to stdout"

compileMode = Compile { inFile = fin def
                      , outFile = fout def
                      , format  = def &= typ "YAML|JSON" &= help "Format of the output file"
                      } &= help "compiles your kk file to either json or yaml"

formatMode = Format { inFile = fin def
                    , outFile = fout def
                    , colour = def &= typ "yes|no" &= help "colourize the output"
                    } &= help "formats your kk file, fixing indenting"

lintMode = Lint { inFile = fin def
                , inPlace = def &= typ "true|false" &= help "Update the file in-place"
                } &= help "lints your kk file"

lspMode = LSP {}
             &= help "enable the language server, for autocomplete and on the fly errors"

mode = cmdArgsMode $ modes [compileMode, formatMode, lintMode, lspMode]
  &= help "to yaml or json configuration compiler"
  &= summary "kk v0.0.0, (C) Ashley Towns"

doCompile Compile{..} = do
  output <- case format of
    YAML -> kCompile inFile
    JSON -> kCompileJson inFile
  case outFile of
    Just fn -> writeFile fn output
    Nothing -> putStrLn output

doFormat Format{..} = kFormat inFile

main :: IO ()
main = do
  m <- cmdArgsRun mode
  case m of
    x@Compile{} -> doCompile x
    x@Format{}  -> doFormat x
