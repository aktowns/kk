module Type where

import           Data.Text        (Text)
import qualified Data.Set as Set
import           Control.Monad.State.Lazy (State, gets, get, modify, evalState, runState)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import           Node

type KKTy = State (HashMap Text Node)


infer :: Node -> Type
infer (KString _)    = TString
infer (KNumber _)    = TNumber
infer (KList xs)     = 
  let ts = Set.fromList $ map infer xs 
      el = if Set.size ts > 1 then TUnion ts else Set.elemAt 0 ts
  in TList el
infer (KHash xs)     = 
  let ts = Set.fromList $ map (infer . snd) xs 
      el = if Set.size ts > 1 then TUnion ts else Set.elemAt 0 ts
  in THash el
infer (KObject n xs) = TObject n $ TIntersection $ Set.fromList $ map (\(n, x) -> (n, infer x)) xs
