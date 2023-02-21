module DeBruijn.LC where

import DeBruijn
import Data.Kind (Type)

type Term :: Ctx -> Type
data Term ctx where
    Var :: Idx ctx -> Term ctx
    App :: Term ctx -> Term ctx -> Term ctx
    Lam :: Term (S ctx) -> Term ctx

type EvalEnv :: Ctx -> Ctx -> Type
type EvalEnv ctx ctx' = Env ctx (Val ctx') 

type Val :: Ctx -> Type
data Val ctx where
    Clo :: EvalEnv ctx ctx' -> Term (S ctx) -> Val ctx'
    Neu :: Lvl ctx -> [Val ctx] -> Val ctx
    Err :: Val ctx

instance Sinkable Val where
    mapLvl _ Err         = Err
    mapLvl f (Clo env t) = Clo (fmap (mapLvl f) env) t
    mapLvl f (Neu l sp)  = Neu (f l) (fmap (mapLvl f) sp)

eval :: EvalEnv ctx ctx' -> Term ctx -> Val ctx'
eval env (Var x)   = lookupEnv x env
eval env (Lam t)   = Clo env t
eval env (App f t) = case eval env f of
    Clo env' f' -> eval (env' :> eval env t) f'
    Neu l sp    -> Neu l (eval env t : sp)
    Err         -> Err

type Nf :: Ctx -> Type
data Nf ctx where
    NLam :: Nf (S ctx) -> Nf ctx
    Neut :: Ne ctx -> Nf ctx

type Ne :: Ctx -> Type
data Ne ctx where
    NVar :: Idx ctx -> Ne ctx
    NApp :: Ne ctx -> Nf ctx -> Ne ctx

quote :: Size ctx -> Val ctx -> Maybe (Nf ctx)
quote _ Err         = Nothing
quote s (Clo env f) = NLam <$> quote (SS s) (eval (mapSink env :> Neu (lvlZ s) []) f)
quote s (Neu l sp)  = Neut . foldr (flip NApp) (NVar (lvlToIdx s l)) <$> traverse (quote s) sp
