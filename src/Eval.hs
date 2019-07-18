module Eval where

import           Data.Text           (Text)
import           Data.Void           (Void)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Bitraversable  (bitraverse)

import           Control.Monad.State.Lazy (State, gets, get, modify, evalState, runState)

import Node

data Ctx = Ctx { env :: HashMap Text ([Text], Node)
               , var :: HashMap Text Node 
               } deriving (Show)

type KK = State Ctx

emptyCtx :: Ctx
emptyCtx = Ctx { env = HM.empty, var = HM.empty }

envInsert :: Text -> ([Text], Node) -> KK ()
envInsert n a = do
    e <- gets env
    modify (\s -> s {env = HM.insert n a e})

envGet :: Text -> KK ([Text], Node)
envGet n = do
    e <- gets env
    return $ fromMaybe (error $ show n ++ " is not defined") (HM.lookup n e)

varGet :: Text -> KK Node
varGet n = do
    v <- gets var
    return $ fromMaybe (error $ show n ++ " is not defined") (HM.lookup n v)

envApply :: Text -> [Node] -> KK (Maybe Node)
envApply n a = do
    (a', n') <- envGet n
    ctx <- get
    let e = ctx { var = HM.fromList $ zip a' a }
    pure $ evalKK e $ reduce n'

reduceFn :: (Text, Node) -> KK (Maybe (Text, Node))
reduceFn (t, x) = do 
  x' <- reduce x
  return $ bitraverse pure id (t, x')

reduce :: Node -> KK (Maybe Node)
reduce (KObject n body) = Just . KObject n . catMaybes <$> mapM reduceFn body
reduce (KList xs)       = Just . KList . catMaybes <$> mapM reduce xs
reduce (KDefine n a b)  = Nothing <$ envInsert n (a, b)
reduce (KTemplate _ _)  = pure Nothing -- Todo
reduce (KCall n a)      = envApply n a
reduce (KVariable n)    = Just <$> varGet n
reduce k                = pure $ Just k

reduceAll :: [Node] -> KK [Node]
reduceAll x = catMaybes <$> mapM reduce x
 
evalKK :: Ctx -> KK a -> a
evalKK ctx kk = evalState kk ctx

runKK :: Ctx -> KK a -> (a, Ctx)
runKK ctx kk = runState kk ctx