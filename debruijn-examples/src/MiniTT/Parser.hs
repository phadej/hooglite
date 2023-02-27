module MiniTT.Parser (
    Stmt (..),  
) where

import MiniTT.Raw

-- | MiniTT statements
data Stmt
    = DefineStmt Name Raw  -- ^ define top level binding: @define foo = e@ or @define bar : T = t@
    | EvalStmt Raw         -- ^ evaluate expression: @eval e@
    | TypeStmt Raw         -- ^ type-check expression: @type e@
  deriving Show