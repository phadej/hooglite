module DeBruijn.LC where

import DeBruijn
import Data.Kind (Type)

-- | Term types are checked
type Term :: Ctx -> Type
data Term ctx where
    Lam :: Term (S ctx) -> Term ctx
    Pie :: Term ctx -> Term (S ctx) -> Term ctx
    Typ :: Term ctx
    Coe :: Elim ctx -> Term ctx

-- | Elimination types are inferred
type Elim :: Ctx -> Type
data Elim ctx where
   Var :: Idx ctx -> Elim ctx
   App :: Elim ctx -> Term ctx -> Elim ctx
   Ann :: Term ctx -> Term ctx -> Elim ctx

deriving instance Show (Term ctx)
deriving instance Show (Elim ctx)

type EvalEnv :: Ctx -> Ctx -> Type
type EvalEnv ctx ctx' = Env ctx (Val ctx') 

type Val :: Ctx -> Type
data Val ctx where
    VLam :: Closure ctx -> Val ctx
    VNeu :: Lvl ctx -> [Val ctx] -> Val ctx
    VTyp :: Val ctx
    VPie :: Val ctx -> Closure ctx -> Val ctx
    VErr :: String -> Val ctx

deriving instance Show (Val ctx)

type Closure :: Ctx -> Type
data Closure ctx' where Closure :: EvalEnv ctx ctx' -> Term (S ctx) -> Closure ctx'

deriving instance Show (Closure ctx)

run :: Closure ctx -> Val ctx -> Val ctx
run (Closure env f) t = eval (env :> t) f

instance Sinkable Closure where
    mapLvl f (Closure env t) = Closure (fmap (mapLvl f) env) t

instance Sinkable Val where
    mapLvl _ (VErr msg)   = VErr msg
    mapLvl _ VTyp         = VTyp
    mapLvl f (VLam clos)  = VLam (mapLvl f clos)
    mapLvl f (VPie a b)   = VPie (mapLvl f a) (mapLvl f b)
    mapLvl f (VNeu l sp)  = VNeu (f l) (fmap (mapLvl f) sp)

eval :: EvalEnv ctx ctx' -> Term ctx -> Val ctx'
eval env (Lam t)   = VLam (Closure env t)
eval _   Typ       = VTyp
eval env (Pie a b) = VPie (eval env a) (Closure env b)
eval env (Coe e)   = eval' env e

eval' :: EvalEnv ctx ctx' -> Elim ctx -> Val ctx'
eval' env (Var x)   = lookupEnv x env
eval' env (Ann t _) = eval env t
eval' env (App f t) = case eval' env f of
    VLam clos    -> run clos (eval env t)
    VNeu l sp    -> VNeu l (eval env t : sp)
    VTyp         -> VErr "Typ applied"
    VPie _ _     -> VErr "Pi applied"
    VErr msg     -> VErr msg

type Nf :: Ctx -> Type
data Nf ctx where
    NLam :: Nf (S ctx) -> Nf ctx
    NPie :: Nf ctx -> Nf (S ctx) -> Nf ctx
    NTyp :: Nf ctx
    Neut :: Ne ctx -> Nf ctx

type Ne :: Ctx -> Type
data Ne ctx where
    NVar :: Idx ctx -> Ne ctx
    NApp :: Ne ctx -> Nf ctx -> Ne ctx

deriving instance Show (Nf ctx)
deriving instance Show (Ne ctx)

val0 :: Size ctx -> Val (S ctx)
val0 s = VNeu (lvlZ s) [] 

quote :: Size ctx -> Val ctx -> Either String  (Nf ctx)
quote _ (VErr msg)   = Left msg
quote s (VLam clos)  = NLam <$> quote (SS s) (run (sink clos) (val0 s))
quote s (VPie a b)   = NPie <$> quote s a <*> quote (SS s) (run (sink b) (val0 s))
quote s (VNeu l sp)  = Neut . foldr (flip NApp) (NVar (lvlToIdx s l)) <$> traverse (quote s) sp
quote _ VTyp         = return NTyp

-- | Beta-eta conversion chekcing
conv :: Size ctx -> Val ctx -> Val ctx -> Bool
conv _ VTyp VTyp = True
conv s (VPie a1 b1) (VPie a2 b2) = conv s a1 a2 && conv (SS s) (run (sink b1) (val0 s)) (run (sink b2) (val0 s))
conv s (VLam b1)    (VLam b2)    = conv (SS s) (run (sink b1) (val0 s)) (run (sink b2) (val0 s))
conv s (VLam b1)    u            = conv (SS s) (run (sink b1) (val0 s)) (sink u)
conv s t            (VLam b2)    = conv (SS s) (sink t) (run (sink b2) (val0 s))
conv s (VNeu x sp1) (VNeu y sp2) = x == y && convSp s sp1 sp2
conv _ _            _            = False

convSp :: Size ctx -> [Val ctx] -> [Val ctx] -> Bool
convSp _ []     []     = True
convSp s (x:xs) (y:ys) = conv s x y && convSp s xs ys
convSp _ _      _      = False

data InferCtx ctx ctx' = InferCtx
    { values :: EvalEnv ctx ctx'
    , types  :: Env ctx (Val ctx')
    , size   :: Size ctx'
    }

emptyInferCtx :: InferCtx EmptyCtx EmptyCtx
emptyInferCtx = InferCtx EmptyEnv EmptyEnv SZ 

bind :: InferCtx ctx ctx' -> Val ctx' -> InferCtx (S ctx) (S ctx') 
bind (InferCtx e t s) x = InferCtx (mapSink e :> val0 s) (mapSink t :> sink x) (SS s)

check :: InferCtx ctx ctx' -> Term ctx -> Val ctx' -> Either String ()
check ctx (Lam t) (VPie a b) = do
    let ctx' = bind ctx a
    check ctx' t (run (sink b) (val0 (size ctx)))
check _   (Lam _) ty         = Left $ "Lam not Pie: " ++ show ty
check ctx (Pie a b) VTyp = do
    check ctx a VTyp
    check (bind ctx (eval (values ctx) a)) b VTyp
check _   (Pie _ _) ty   = Left $ "Pie not Typ: " ++ show ty
check _   Typ       VTyp = pure ()
check _   Typ       ty   = Left $ "Typ not Typ: " ++ show ty
check ctx (Coe e)   a    = do
    b <- infer ctx e
    if conv (size ctx) a b
    then pure ()
    else Left $ "Cannot convert: " ++ show (quote (size ctx) a, quote (size ctx) b)

infer :: InferCtx ctx ctx' -> Elim ctx -> Either String (Val ctx')
infer ctx (Var x)   = return (lookupEnv x (types ctx))
infer ctx (App f x) = do
    fty <- infer ctx f
    case fty of
        VPie a b -> do
            check ctx x a
            return (run b (eval (values ctx) x))
        _ -> Left $ "Application had not Pie" ++ show fty
infer ctx (Ann t s) = do
    check ctx s VTyp
    let s' = eval (values ctx) s
    check ctx t s'
    return s'

exampleId :: Elim EmptyCtx
exampleId = Ann
    (Lam $ Lam $ Coe $ Var IZ)
    (Pie Typ $ Pie (Coe (Var IZ)) (Coe (Var (IS IZ))))

exampleWrong :: Elim EmptyCtx
exampleWrong = Ann
    (Lam $ Coe $ Var IZ)
    (Pie Typ $ Pie (Coe (Var IZ)) (Coe (Var (IS IZ))))
