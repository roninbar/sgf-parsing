module Sgf (parseSgf) where
import qualified Sgf.Internal (lexer, parseSgf)
import Data.Tree (Tree)
import Data.Map (Map)
import Data.Text (Text)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: String -> Maybe SgfTree
parseSgf = Sgf.Internal.parseSgf . Sgf.Internal.lexer

