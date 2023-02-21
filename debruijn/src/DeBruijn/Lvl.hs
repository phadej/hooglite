module DeBruijn.Lvl (
    Lvl,
    lvlToIdx,
    sinkLvl,
    lvlZ,
    Sinkable (..),
    sink,
    mapSink,
) where

import DeBruijn.Internal.Lvl