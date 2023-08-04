module Sgf
  ( parseSgf
  ) where

import           Control.Applicative  (Alternative (..))
import           Data.Char            (isUpper)
import           Data.Map             (Map, fromList)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Tree            (Tree (..))
import           Data.Void            (Void)
import           Maybes               (rightToMaybe)
import           Text.Megaparsec      (MonadParsec (..), Parsec, anySingle,
                                       anySingleBut, between, manyTill,
                                       runParser, satisfy)
import           Text.Megaparsec.Char (char)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

type Parser = Parsec Void Text

propName :: Parser Text
propName = T.pack <$> some (satisfy isUpper)

propValue :: Parser Text
propValue = T.pack <$ char '[' <*> fmap concat (manyTill charLiteral $ char ']')
  where
    charLiteral :: Parser String
    charLiteral =
      convert [('\t', " ")] <$> anySingleBut '\\' <|>
      convert [('\t', " "), ('\n', "")] <$ char '\\' <*> anySingle
    convert t c = fromMaybe [c] (lookup c t)

prop :: Parser (Text, [Text])
prop = (,) <$> propName <*> some propValue

node :: Parser SgfNode
node = fromList <$ char ';' <*> many prop

branch :: Parser SgfTree
branch =
  try ((\x y -> Node x [y]) <$> node <*> branch) <|> Node <$> node <*> many tree

tree :: Parser SgfTree
tree = between (char '(') (char ')') branch

parseSgf :: String -> Maybe SgfTree
parseSgf = rightToMaybe . runParser (tree <* eof) "" . T.pack
