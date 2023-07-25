{
module Sgf.Internal (lexer, parseSgf) where
import Data.Char (isSpace, isAlpha, isAlphaNum, isUpper)
import Data.Map  (Map, empty, singleton, insert)
import Data.Text (Text, pack, unpack, strip)
import Data.Tree (Tree(..))
}

%name parseSgf
%tokentype { Token }
%error { parseError }
%monad { Maybe }

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
        | Prop                  { singleton (head $1) (tail $1) }
        | Prop Props            { insert (head $1) (tail $1) $2 }
Prop    : name Values           { pack $1 : $2 }
Values  : value                 { [pack $1] }
        | value Values          { pack $1 : $2 }

{
parseError :: [Token] -> Maybe a
parseError _ = Nothing

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
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer ""                    = []
lexer ('(':cs)              = ParenOpen : lexer cs
lexer (')':cs)              = ParenClose : lexer cs
lexer (';':cs)              = Semicolon : lexer cs
lexer ('[':cs)              = let (value, rest) = span (/=']') cs in PropValue (unpack $ strip $ pack value) : lexer (tail rest)
lexer s@(c:cs)  | isAlpha c = let (name, rest) = span isAlphaNum s in PropName name : lexer rest
                | isSpace c = lexer cs
                | otherwise = error $ "Unexpected character: '" ++ [c] ++ "'"
}