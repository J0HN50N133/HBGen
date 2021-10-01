module Markup
    ( Document
    , Structure(..)
    )
where

import Numeric.Natural

type Document
  = [Structure]
data Structure
  = Header  Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrdereredList [String]
  | CodeBlock [String]
  deriving Show

