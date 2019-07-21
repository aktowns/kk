module Eval where

import           Control.Monad.State.Lazy (State, evalState, get, gets, modify, runState)
import           Data.Bitraversable       (bitraverse)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Text                (Text)
import           Data.Void                (Void)

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
reduce (KObject p t n body) = Just . KObject p t n . catMaybes <$> mapM reduceFn body
reduce (KList p t xs)       = Just . KList p t . catMaybes <$> mapM reduce xs
reduce (KDefine _ _ n a b)  = Nothing <$ envInsert n (a, b)
--reduce (KTemplate _ _ _ _)  = pure $ Just
reduce (KCall _ _ n a)      = envApply n a
reduce (KVariable _ _ n)    = Just <$> varGet n
reduce k                    = pure $ Just k

reduceAll :: [Node] -> KK [Node]
reduceAll x = catMaybes <$> mapM reduce x

evalKK :: Ctx -> KK a -> a
evalKK ctx kk = evalState kk ctx

runKK :: Ctx -> KK a -> (a, Ctx)
runKK ctx kk = runState kk ctx
