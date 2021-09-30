module Html
    ( Html
    , Title
    , Node
    , html_
    , p_
    , concat_
    , render
    ) where
import Data.Char
import Text.Printf ( printf )


-- level 0
data TagType = HTML | BODY | TITLE | HEAD | H1 | P deriving(Show)

el :: TagType -> String -> String
el t = surround (map toLower $ show t)
    where
        surround tag content =
            printf "<%s>%s</%s>" tag content tag

-- level 1
newtype Html
  = Html String
newtype Node
  = Node String
type Title
  = String
getNodeString :: Node -> String
getNodeString (Node str) = str

html_ :: Title -> Node -> Html
html_  title content =
    Html (
        el HTML (
            el HEAD (el TITLE title)
            <> el BODY (getNodeString content)
        )
    )
body_  = Node . el BODY
title_ = Node . el TITLE
head_  = Node . el HEAD
p_     = Node . el P
h1_    = Node . el H1
concat_ (Node s1) (Node s2) = Node (s1 <> s2)

-- level 2
myhtml =
    html_
        "My title"
        (concat_
            (h1_ "Header")
            (concat_
                (p_ "Paragraph #1")
                (p_ "Paragraph #2")))
render html =
    case html of
      Html str -> str

-- level 3
main = putStrLn (render myhtml)
