{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import           Control.Monad.State.Lazy (State, evalState, get, gets, modify, runState)
import           Data.Bifunctor           (bimap)
import           Data.Bitraversable       (bitraverse)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Void                (Void)

import Debug.Trace (trace)

import Node

data Ctx = Ctx { env :: HashMap Text ([Text], Node Position Check)
               , var :: HashMap Text (Node Position Check)
               } deriving (Show)

type KK = State Ctx

emptyCtx :: Ctx
emptyCtx = Ctx { env = HM.empty, var = HM.empty }

ctxFromKV :: [(Text, Text)] -> Ctx
ctxFromKV vars = Ctx { env = HM.empty
                     , var = HM.fromList vars'
                     }
  where
    vars' = map (\(k,v) -> (T.append "env:" k, KString (Position 0 0 "environment") Untyped Literal [] v)) vars

envInsert :: Text -> ([Text], Node Position Check) -> KK ()
envInsert n a = do
  e <- gets env
  modify (\s -> s {env = HM.insert n a e})

varInsert :: Text -> Node Position Check -> KK ()
varInsert n a = do
  e <- gets var
  modify (\s -> s {var = HM.insert n a e})

envGet :: Text -> KK ([Text], Node Position Check)
envGet n = do
  e <- gets env
  return $ fromMaybe (error $ show n ++ " is not defined") (HM.lookup n e)

varGet :: Text -> KK (Node Position Check)
varGet n = do
  v <- gets var
  return $ fromMaybe (error $ show n ++ " is not defined. scope: " ++ show v) (HM.lookup n v)

envApply :: Text -> [Node Position Check] -> KK (Maybe (Node Position Check))
envApply n a = do
  (a', n') <- envGet n
  ctx <- get
  let e = ctx { var = HM.union (HM.fromList $ zip a' a) (var ctx) }
  pure $ evalKK e $ reduce n'

reduceFn :: (Text, Node Position Check) -> KK (Maybe (Text, Node Position Check))
reduceFn (t, x) = do
  x' <- reduce x
  return $ bitraverse pure id (t, x')

forceString :: Maybe (Node a b) -> Text
forceString (Just (KString _ _ _ _ s)) = s
forceString _                          = error "A type of string was expected"

reduceString :: Text -> [(Text, Maybe (Node a b))] -> Text
reduceString = foldl (\b (x,v) -> T.replace x (forceString v) b)

reduce :: Node Position Check -> KK (Maybe (Node Position Check))
reduce (KObject p t n body) = Just . KObject p t n . catMaybes <$> mapM reduceFn body
reduce (KList p t xs)       = Just . KList p t . catMaybes <$> mapM reduce xs
reduce (KHash p t xs)       = Just . KHash p t . catMaybes <$> mapM reduceFn xs
reduce (KDefine _ _ n a b)  =
  Nothing <$ case a of
    (Just a') -> envInsert n (a', b)
    Nothing   -> varInsert n b
reduce KTemplate{}  = pure Nothing
reduce (KCall _ _ n a)      = envApply n a
reduce (KVariable _ _ n)    = reduce =<< varGet n
reduce (KString p t n i s)  = do
  vars <- traverse (bitraverse pure reduce) i
  return $ Just . KString p t n [] $ reduceString s vars
reduce KComment{}    = pure Nothing
reduce (KNumber p t v)      = return $ Just $ KNumber p t v
reduce k                    = error $ "reducer uncaught: " ++ show k

reduceAll :: [Node Position Check] -> KK [Node Position Check]
reduceAll x = catMaybes <$> mapM reduce x

evalKK :: Ctx -> KK a -> a
evalKK ctx kk = evalState kk ctx

runKK :: Ctx -> KK a -> (a, Ctx)
runKK ctx kk = runState kk ctx
