module Html.Internal where
import Data.Char
import Text.Printf ( printf )


-- level 0 * TagType
data TagType = HTML | BODY | TITLE | HEAD | H1 | P deriving(Show)

-- level 1: EDSL Type
newtype Html
  = Html String
newtype Node
  = Node String
type Title
  = String
getNodeString :: Node -> String
getNodeString (Node str) = str

-- level 2: EDSL
html_ :: Title -> Node -> Html
html_  title content =
    Html (
        el HTML (
            el HEAD (el TITLE title)
            <> el BODY (getNodeString content)
        )
    )
body_  = Node . el BODY
head_  = Node . el HEAD
title_ = Node . el TITLE . escape
p_     = Node . el P . escape
h1_    = Node . el H1 . escape
concat_ (Node s1) (Node s2) = Node (s1 <> s2)

-- level 3: Render
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
