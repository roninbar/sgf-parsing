{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Sgf.Internal (parseSgf) where
import Data.Char (isSpace, isAlpha, isAlphaNum, isUpper)
import Data.Map  (Map, empty, singleton, insert)
import Data.Text (Text, pack, unpack, strip)
import Data.Tree (Tree(..))
import Control.Monad.State ( MonadState(..), StateT(..) )
}

%name parseSgf
%tokentype { Token }
%error { parseError }
%monad { P }
%lexer { lexer } { EOF }

%token
    ';'                       { Semicolon }
    '('                       { ParenOpen }
    ')'                       { ParenClose }
    name                      { PropName $$ }
    value                     { PropValue $$ }

%%

-- (;FF[4](;B[aa];W[ab])(;B[dd];W[ee]))

Tree    : '(' Branch ')'        { $2 }
Branch  : Node Forest           { Node $1 $2 }
        | Node Branch           { Node $1 [$2] }
Forest  : {- empty -}           { [] }
        | Tree Forest           { $1 : $2 }
Node    : ';' Props             { $> }
Props   : {- empty -}           { empty :: SgfNode }
        | Prop Props            { insert (head $1) (tail $1) $2 }
Prop    : name Values           { pack $1 : $2 }
Values  : value                 { [pack $1] }
        | value Values          { pack $1 : $2 }

{
parseError :: [Token] -> P a
parseError _ = StateT $ const Nothing

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

data Token 
    = Semicolon 
    | ParenOpen 
    | ParenClose 
    | PropName String
    | PropValue String
    | EOF
    deriving (Show, Eq)

type P = StateT String Maybe

lexer :: (Token -> P a) -> P a
lexer = (lexer' >>=)

lexer' :: P Token
lexer' = get >>= \case
  ""         -> return EOF
  ('(' : cs) -> put cs >> return ParenOpen
  (')' : cs) -> put cs >> return ParenClose
  (';' : cs) -> put cs >> return Semicolon
  ('[' : cs) ->
    let (value, rest) = span (/= ']') cs
    in  put (tail rest) >> return (PropValue value)
  s@(c : cs)
    | isAlpha c
    -> let (name, rest) = span isAlphaNum s
       in  put rest >> return (PropName name)
    | isSpace c
    -> put cs >> lexer'
    | otherwise
    -> parseError [PropName [c]]
}