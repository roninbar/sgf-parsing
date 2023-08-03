module Sgf
  ( parseSgf
  ) where

import           Control.Monad.State (evalStateT)
import           Data.Map            (Map)
import           Data.Text           (Text)
import           Data.Tree           (Tree)
import           Sgf.Internal        (parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: String -> Maybe SgfTree
parseSgf = evalStateT parser
