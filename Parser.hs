module Parser where

import Markup

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
    let
        paragraph = Paragraph (unlines (reverse currentParagraph))
        -- reverse cuz we accumulate result at head
    in
        case txts of
          [] -> [paragraph]
          currentLine : rest ->
              if trim currentLine == ""
                 then
                    paragraph : parseLines [] rest
                 else
                    parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words
