{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Sgf.Internal (parseSgf, lexer, lexer') where
import Data.Char (isSpace, isUpper)
import Data.Map  (Map, empty, singleton, insert)
import Data.Text (Text, pack, unpack, strip)
import Data.Tree (Tree(..))
import Control.Monad.State ( MonadState(..), StateT(..) )
import Text.Regex.Base  ( AllMatches(getAllMatches)
                        , MatchLength
                        , MatchOffset
                        )
import Text.Regex.PCRE ( (=~) )
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
parseError :: Token -> P a
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
  s@('[' : cs) ->
    let (_, _, rest, [value]) = s =~ "^\\[((?:[^\\]\\\\]|\\\\.)*)\\]" :: (String, String, String, [String])
    in  put rest >> return (PropValue $ replace '\t' ' ' $ unescape value)
  s@(c : cs)
    | isUpper c
    -> let (name, rest) = span isUpper s
       in  put rest >> return (PropName name)
    | isSpace c
    -> put cs >> lexer'
    | otherwise
    -> parseError $ PropName [c] 
  where
    replace :: Char -> Char -> String -> String
    replace c' c'' s = map (\c -> if c == c' then c'' else c) s
    unescape :: String -> String
    unescape s =
      let ms =
            getAllMatches (s =~ "\\\\." :: AllMatches [] (MatchOffset, MatchLength))
      in  foldr (\(o, _) s' -> take o s' ++ drop (o + 1) s') s ms
}