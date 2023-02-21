module DeBruijn.Env (
    Env (EmptyEnv, (:>)),
    lookupEnv,
    sizeEnv, 
) where

import DeBruijn.Internal.Env
