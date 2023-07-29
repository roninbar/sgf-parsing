{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Sgf
  ( parseSgf
  , lexer'
  ) where
import           Control.Applicative            ( Alternative( (<|>)) )
import           Control.Monad.State            ( MonadState(..)
                                                , StateT(..)
                                                , evalStateT
                                                )
import           Data.Char                      ( isSpace
                                                , isUpper
                                                )
import           Data.Map                       ( (!?)
                                                , Map
                                                , fromList
                                                )
import           Data.Text                      ( Text )
import           Data.Tree                      ( Tree )
import qualified Sgf.Internal                   ( parseSgf )
import           Text.RawString.QQ
import           Text.Regex.Base                ( AllMatches(..)
                                                , MatchLength
                                                , MatchOffset
                                                , RegexContext(..)
                                                , RegexMaker(..)
                                                , RegexOptions(..)
                                                )
import           Text.Regex.PCRE                ( (=~)
                                                , Regex
                                                , compDotAll
                                                )

type P = StateT String Maybe

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: String -> Maybe SgfTree
parseSgf = evalStateT Sgf.Internal.parseSgf

parseError :: Token -> P a
parseError _ = StateT $ const Nothing

data Token
    = Semicolon
    | ParenOpen
    | ParenClose
    | PropName String
    | PropValue String
    | EOF
    deriving (Show, Eq)

lexer' :: P Token
lexer' = get >>= \case
  ""         -> return EOF
  ('(' : cs) -> put cs >> return ParenOpen
  (')' : cs) -> put cs >> return ParenClose
  (';' : cs) -> put cs >> return Semicolon
  s@('[' : _) ->
    let regex =
          makeRegexOpts (defaultCompOpt + compDotAll)
                        defaultExecOpt
                        "^\\[((?:[^\\]\\\\]|\\\\.)*)\\]" :: Regex
        (_, _, rest, [value]) =
          match regex s :: (String, String, String, [String])
    in  put rest >> return
          (PropValue $ replace '\t' ' ' $ replaceEscape
            (fromList [('\t', " "), ('\n', "")])
            value
          )
  s@(c : cs)
    | isUpper c
    -> let (name, rest) = span isUpper s in put rest >> return (PropName name)
    | isSpace c
    -> put cs >> lexer'
    | otherwise
    -> parseError $ PropName [c]
 where
  replace :: Char -> Char -> String -> String
  replace c' c'' = map (\c -> if c == c' then c'' else c)
  replaceEscape :: Map Char String -> String -> String
  replaceEscape t s =
    let ms = (s =~ [r|\\.|] :: AllMatches [] (MatchOffset, MatchLength))
    in  foldr
          (\(o, _) s' ->
            let c       = s' !! (o + 1)
                Just c' = (t !? c) <|> Just [c]
            in  take o s' ++ c' ++ drop (o + 2) s'
          )
          s
          (getAllMatches ms)

