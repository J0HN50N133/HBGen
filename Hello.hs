import Html

-- level 2
myhtml =
    html_
        "My title"
        (concat_
            [ h1_ "Header"
            , ol_
                [ p_ "OrderIterm1"
                , p_ "OrderIterm2"
                , p_ "OrderIterm3"]
            , ul_
                [ p_ "UnorderItem1"
                , p_ "UnorderItem2"
                , p_ "UnorderItem3"]
            , code_
                "#include <stdio.h>\n\
                \int main(int argc, const char* argv[]){\n\
                \    printf(\"Hello world!\");\n\
                \}"
            ])

-- level 3
main = putStrLn (render myhtml)
