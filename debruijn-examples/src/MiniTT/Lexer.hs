module MiniTT.Lexer (
    Token (..),
    LexerState (..),
    initLexerState,
    scan,  
) where

import Data.Text.Short (ShortText)
import Data.ByteString (ByteString)

import qualified Text.Parsec as P
import qualified Data.ByteString as BS

-- | Tokens produced by lexer.
data Token
    = TkIdent ShortText  -- ^ identifiers: @foobar@
    | TkDefine           -- ^ keyword @define@
    | TkEval             -- ^ keyword @eval@
    | TkType             -- ^ keyword @type@
    | TkLet              -- ^ keyword @let@
    | TkIn               -- ^ keyword @in@
    | TkLParen           -- ^ left parenthesis: @(@
    | TkRRaren           -- ^ right parenthesis: @)@
    | TkSemi             -- ^ semicolon: @;@
    | TkDot              -- ^ dot: @.@
    | TkArrow            -- ^ arrow: @->@
    | TkBackSlash        -- ^ backslash: @\\@
    | TkColon            -- ^ colon: @:@
    | TkEquals           -- ^ equals: @=@
    | TkEOF              -- ^ end-of-file token
    | TkError String
  deriving (Eq, Show)

data LexerState = LS
    { lsContents :: {-# UNPACK #-} !ByteString
    }

instance Monad m => P.Stream LexerState m Token where
    uncons (stripSpace -> ls)
        | BS.null (lsContents ls) = return Nothing
        | otherwise               = case scan ls of
            (TkEOF, _) -> return Nothing
            res        -> return (Just res)

initLexerState :: FilePath -> ByteString -> LexerState
initLexerState _fn bs = LS
    { lsContents = bs
    }

stripSpace :: LexerState -> LexerState
stripSpace = id -- TODO

scan :: LexerState -> (Token, LexerState)
scan ls@(LS _contents) = (TkError "TODO", ls)


