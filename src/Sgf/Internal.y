{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Sgf.Internal (parseSgf, lexer, lexer') where
import Control.Monad.State ( MonadState(..), StateT(..) )
import Data.Char (isSpace, isUpper)
import Data.Map  ( (!?), Map, findWithDefault, fromList, empty, singleton, insert)
import Data.Text (Text, pack, unpack, strip)
import Data.Tree (Tree(..))
import Text.RawString.QQ
import Text.Regex.Base  ( AllMatches(..)
                        , MatchLength
                        , MatchOffset
                        , RegexContext(..)
                        , RegexMaker(..)
                        , RegexOptions(..)
                        )
import Text.Regex.PCRE ( (=~), Regex, compDotAll )
}

%name                           parseSgf
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
    let regex = makeRegexDotAll [r|^\[((?:[^]\\]|\\.)*)\]|]
        (_, _, rest, [value]) = match regex s :: (String, String, String, [String])
    in put rest >> return (PropValue  $ replace '\t' ' ' 
                                      $ replaceEscape (fromList [('\t', " "), ('\n', "")]) 
                                      value)
  s@(c : cs)
    | isUpper c
    -> let (name, rest) = span isUpper s
       in  put rest >> return (PropName name)
    | isSpace c
    -> put cs >> lexer'
    | otherwise
    -> parseError EOF 
  where
    makeRegexDotAll :: String -> Regex
    makeRegexDotAll = makeRegexOpts (defaultCompOpt + compDotAll) defaultExecOpt
    replace :: Char -> Char -> String -> String
    replace c' c'' = map (\c -> if c == c' then c'' else c)
    replaceEscape :: Map Char String -> String -> String
    replaceEscape t s =
      let regex = makeRegexDotAll [r|\\.|]
          ms = getAllMatches (match regex s) :: [(MatchOffset, MatchLength)]
      in  foldr (\(o, l) s' -> -- replace one escape sequence. l should always be 2.
                  let c  = s' !! (o + 1) -- the character following the \.
                      c' = findWithDefault [c] c t
                  in  take o s' ++ c' ++ drop (o + l) s')
                s
                ms

parseError :: Token -> M a
parseError _ = StateT $ const Nothing
}