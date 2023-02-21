module DeBruijn.Lvl (
    Lvl,
    lvlToIdx,
    lvlZ,
    sinkLvl,  
) where

import Data.Kind (Type)

import DeBruijn.Ctx
import DeBruijn.Idx
import DeBruijn.Size

type Lvl :: Ctx -> Type
type role Lvl nominal
newtype Lvl ctx = MkLvl (forall ctx'. Size ctx' -> LE ctx ctx' -> Idx ctx')

type LE :: Ctx -> Ctx -> Type
data LE ctx ctx' where
    LERefl :: LE ctx ctx
    LENext :: LE ctx ctx' -> LE ctx (S ctx')

lePrev :: LE (S ctx) ctx' -> LE ctx ctx'
lePrev LERefl     = LENext LERefl
lePrev (LENext p) = LENext $ lePrev p

lvlToIdx :: Size ctx -> Lvl ctx -> Idx ctx
lvlToIdx s (MkLvl f) = f s LERefl 

sinkLvl :: Lvl ctx -> Lvl (S ctx)
sinkLvl (MkLvl f) = MkLvl (\size le -> f size (lePrev le))

lvlZ :: Size ctx -> Lvl (S ctx)
lvlZ size = MkLvl $ go size where
    go :: Size ctx -> Size ctx' -> LE (S ctx) ctx' -> Idx ctx'
    go ctx _         LERefl      = sizeToIdx ctx
    go ctx (SS ctx') (LENext le) = IS $ go ctx ctx' le

sizeToIdx :: Size ctx -> Idx (S ctx)
sizeToIdx SZ     = IZ
sizeToIdx (SS n) = IS (sizeToIdx n)
