module Sgf
  ( parseSgf
  ) where

import           Control.Applicative  (Alternative (..))
import           Data.Char            (isUpper)
import           Data.Functor         ((<&>))
import           Data.Map             (Map, fromList)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Tree            (Tree (..))
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (..), Parsec, anySingle,
                                       anySingleBut, manyTill, satisfy, parse)
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
propValue = T.pack . concat <$ char '[' <*> manyTill charLiteral (char ']')
  where
    charLiteral :: Parser String
    charLiteral =
      (anySingleBut '\\' <&> convert [('\t', " ")]) <|>
      (char '\\' *> anySingle <&> convert [('\t', " "), ('\n', "")])
    convert t c = fromMaybe [c] (lookup c t)

prop :: Parser (Text, [Text])
prop = (,) <$> propName <*> some propValue

node :: Parser SgfNode
node = fromList <$ char ';' <*> many prop

branch :: Parser SgfTree
branch =
  try
    (do n <- node
        b <- branch
        return $ Node n [b]) <|>
  Node <$> node <*> many tree

tree :: Parser SgfTree
tree = char '(' *> branch <* char ')'

parseSgf :: String -> Maybe SgfTree
parseSgf = rightToMaybe . parse (tree <* eof) "<String>" . T.pack
  where
    rightToMaybe (Left _)  = Nothing
    rightToMaybe (Right t) = Just t
