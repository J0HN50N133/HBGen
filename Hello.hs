import Html

-- level 2
myhtml =
    html_
        "My title"
        (concat_
            (h1_ "Header")
            (concat_
                (p_ "Paragraph #1")
                (p_ "Paragraph #2")))
-- level 3
main = putStrLn (render myhtml)
