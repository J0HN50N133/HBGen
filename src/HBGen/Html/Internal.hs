module HBGen.Html.Internal where
import Data.Char
import Text.Printf ( printf )
import Numeric.Natural
import Data.Foldable


-- level 0 * TagType
data TagType = HTML | BODY | TITLE | HEAD | H1 | H2 | H3 | H4 | H5 | H6 | P | UL | OL | LI | PRE deriving(Show)

-- level 1: EDSL Type
newtype Html
  = Html String
newtype Node
  = Node String
type Title
  = String

getNodeString :: Node -> String
getNodeString (Node str) = str

instance Semigroup Node where
  (<>) n1 n2 =
    Node (getNodeString n1 <> getNodeString n2)
instance Monoid Node where
  mempty = empty_

-- level 2: EDSL
html_ :: Title -> Node -> Html
html_  title content =
    Html (
        el HTML (
            el HEAD (el TITLE title)
            <> el BODY (getNodeString content)))

body_ :: String -> Node
body_  = Node . el BODY

head_ :: String -> Node
head_  = Node . el HEAD

title_ :: String -> Node
title_ = Node . el TITLE . escape

p_ :: String -> Node
p_     = Node . el P . escape

h_ :: Natural -> String -> Node
h_ n =Node . (case n of
                1 -> el H1
                2 -> el H2
                3 -> el H3
                4 -> el H4
                5 -> el H5
                _ -> el H6) . escape

code_ :: String -> Node
code_  = Node . el PRE . escape

ul_ :: [Node] -> Node
ul_    = Node . el UL . concatMap (el LI . getNodeString)

ol_ :: [Node] -> Node
ol_    = Node . el OL . concatMap (el LI . getNodeString)

empty_ :: Node
empty_ = Node ""

concat_ :: [Node] -> Node
concat_ = fold

-- level 3: Render
render :: Html -> String
render html =
    case html of
      Html str -> str

-- Utilities
escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
              '<'  -> "&lt;"
              '>'  -> "&gt;"
              '&'  -> "&amp;"
              '"'  -> "&quot;"
              '\'' -> "&#39;"
              _    -> [c]
    in
        concatMap escapeChar

el :: TagType -> String -> String
el t = surround (map toLower $ show t)
    where
        surround tag content =
            printf "<%s>%s</%s>" tag content tag
