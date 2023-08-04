{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Sgf.Internal (parser) where
import Control.Monad.State (MonadState(..), StateT(..))
import Data.Char (isSpace, isUpper)
import Data.Map  (Map, findWithDefault, fromList, empty, insert)
import Data.Text (Text, pack)
import Data.Tree (Tree(..))
import Text.Regex.Base  ( AllMatches(..)
                        , MatchLength
                        , MatchOffset
                        , RegexContext(..)
                        , RegexMaker(..)
                        , RegexOptions(..)
                        )
import Text.Regex.TDFA (Regex, CompOption(..))
}

%name                           parser
%tokentype                      { Token }
%error                          { parseError }
%monad                          { M }
%lexer                          { lexer } { EOF }

%token
    ';'                         { Semicolon }
    '('                         { ParenOpen }
    ')'                         { ParenClose }
    name                        { PropName $$ }
    value                       { PropValue $$ }

%%

Tree    : '(' Branch ')'        { $2 }              -- SgfTree
Branch  : Node Forest           { Node $1 $2 }      -- SgfTree
        | Node Branch           { Node $1 [$2] }
Forest  : {- empty -}           { [] }              -- [SgfTree]
        | Tree Forest           { $1 : $2 }
Node    : ';' Props             { fromList $2 }     -- SgfNode
Props   : {- empty -}           { [] }              -- [(Text, [Text])]
        | Prop Props            { $1 : $2 }
Prop    : name Values           { (pack $1, $2) }   -- (Text, [Text])
Values  : value                 { [pack $1] }       -- [Text]
        | value Values          { pack $1 : $2 }

{
type M = StateT String Maybe

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

data Token
    = EOF
    | Semicolon
    | ParenOpen
    | ParenClose
    | PropName String
    | PropValue String
    deriving (Show, Eq)

-- | An adapter that exposes a continuation-passing interface as required by the generated parser.
lexer :: (Token -> M a) -> M a
lexer f = lexer' >>= f

lexer' :: M Token
lexer' = get >>= \case
  ""         -> return EOF
  (';' : cs) -> put cs >> return Semicolon
  ('(' : cs) -> put cs >> return ParenOpen
  (')' : cs) -> put cs >> return ParenClose
  s@('[' : _) -> -- [<value>]
    let (_, _, rest, [value, _]) =
          matchRegexDotAll "^\\[(([^]\\\\]|\\\\.)*)\\]" s :: (String, String, String, [String])
    in put rest >> return (PropValue  $ replace '\t' ' ' 
                                      $ replaceEscape (fromList [('\t', " "), ('\n', "")]) 
                                      $ value)
  s@(c : cs)
    | isUpper c
    -> let (name, rest) = span isUpper s
       in  put rest >> return (PropName name)
    | isSpace c
    -> put cs >> lexer'
    | otherwise
    -> parseError EOF 

matchRegexDotAll :: RegexContext Regex String c => String -> String -> c
matchRegexDotAll = match . makeRegexOpts (defaultCompOpt { multiline = False }) defaultExecOpt

replace :: Char -> Char -> String -> String
replace c' c'' = map (\c -> if c == c' then c'' else c)

replaceEscape :: Map Char String -> String -> String
replaceEscape t s =
  let ms = getAllMatches (matchRegexDotAll "\\\\." s) :: [(MatchOffset, MatchLength)]
  in  foldr (\(o, l) s' -> -- replace one escape sequence. l should always be 2.
              let c = s' !! (o + 1) -- the character following the \.
                  r = findWithDefault [c] c t
              in  splice o l r s')
            s
            ms

splice :: Int -> Int -> String -> String -> String
splice o l r s = take o s ++ r ++ drop (o + l) s

parseError :: Token -> M a
parseError _ = StateT $ const Nothing
}