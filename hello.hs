
import Data.Char
import Text.Printf ( printf )


-- level 0
data TagType = HTML | BODY | TITLE | HEAD | H1 | P deriving(Show)


surround tag content = printf "<%s>%s</%s>" tag content tag
el :: TagType -> String -> String
el t = surround (map toLower $ show t)

-- level 1
html_  = el HTML
body_  = el BODY
title_ = el TITLE
head_  = el HEAD
p_     = el P
h1_    = el H1

-- level 2
makeHtml title content = 
    html_ (head_ (title_ title) <> body_ content)

-- level 3
myHtml =
    makeHtml
        "Hello title"
        (h1_ "Hello, world!" <> p_ "This is a new paragraph!")

main = putStrLn myHtml

