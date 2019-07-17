{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib

import System.Console.CmdArgs

data KK = KK { infile  :: FilePath 
             , outfile :: FilePath
             } deriving (Show, Data, Typeable)

kk = KK { infile  = def &= args &= typ "IN FILE" 
        , outfile = def &= args &= typ "OUT FILE"
        } &= 
        verbosity &=
        help "kk compiler" &=
        summary "kk v0.0.0, (C) Ashley Towns" &=
        details [""]

mode = cmdArgsMode kk

main :: IO ()
main = print =<< cmdArgsRun mode
