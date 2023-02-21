{-# LANGUAGE CPP #-}
-- | de Bruijn indices for well-scoped terms.
module DeBruijn.Idx (
    -- * Idxiables
    Idx (IZ,IS),
    absurdIdx,
    unIdx,
    indexIdx,
    -- ** Common patterns
    unusedIdx,
    unusedIdx2,
    unusedIdx3,
) where


import DeBruijn.Internal.Idx