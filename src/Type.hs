module Type where

import           Control.Monad.State.Lazy (State, evalState, get, gets, modify, runState)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Set                 as Set
import           Data.String.Conv         (toS)
import           Data.Text                (Text)

import Node

data TypeStatus = CheckOk | CheckFail String deriving (Show)

type KKTy = State (HashMap Text Node)

apply :: Node -> Type -> Node
apply (KString p _ h i s) t   = KString p (Typed t) h i s
apply (KNumber p _ s) t   = KNumber p (Typed t) s
apply (KBool p _ s) t     = KBool p (Typed t) s
apply (KList p _ s) t     = KList p (Typed t) s
apply (KHash p _ s) t     = KHash p (Typed t) s
apply (KObject p _ n s) t = KObject p (Typed t) n s

typed :: Node -> Node
typed x@(KString _ Untyped _ _ _)    = apply x $ infer x
typed x@(KNumber _ Untyped _)    = apply x $ infer x
typed x@(KBool _ Untyped _)      = apply x $ infer x
typed x@(KList p Untyped xs)     = KList p (Typed $ infer x) $ map typed xs
typed x@(KHash p Untyped xs)     = KHash p (Typed $ infer x) $ map (\(x, y) -> (x, typed y)) xs
typed x@(KObject p Untyped n xs) = KObject p (Typed $ infer x) n $ map (\(n, x) -> (n, typed x)) xs

infer :: Node -> Type
infer KString{}    = TString
infer KNumber{}    = TNumber
infer KBool{}      = TBool
infer (KList _ _ xs)     =
  let ts = Set.fromList $ map infer xs
      el = if Set.size ts > 1 then TUnion ts else Set.elemAt 0 ts
  in TList el
infer (KHash _ _ xs)     =
  let ts = Set.fromList $ map (infer . snd) xs
      el = if Set.size ts > 1 then TUnion ts else Set.elemAt 0 ts
  in THash el
infer (KObject _ _ n xs) =
  TObject n $ TIntersection $ Set.fromList $ map (\(n, x) -> (n, infer x)) xs

typeOf :: Type -> String
typeOf TString       = "String"
typeOf TNumber       = "Number"
typeOf TBool         = "Bool"
typeOf (TList x)     = "[" ++ typeOf x ++ "]"
typeOf (THash x)     = "{" ++ typeOf x ++ "}"
typeOf (TObject n _) = toS n

check :: Type -> Type -> TypeStatus
check TString TString = CheckOk
check x       y       = CheckFail $ "Expected: " ++ typeOf x ++ " but got " ++ typeOf y
