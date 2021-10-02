module Parser where

import Data.Maybe
import Markup

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
        case txts of
          [] -> maybeToList context
          currentLine : rest ->
            let
              line = trim currentLine
            in
              if line == ""
                 then
                    maybe id (:) context (parseLines Nothing rest)
                 else
                    case context of
                      Just (Paragraph p) -> 
                        -- unwords here is extremely slow
                        parseLines (Just (Paragraph (unwords [p, line]))) rest
                      _ ->
                        maybe id (:) context (parseLines (Just (Paragraph line)) rest)


trim :: String -> String
trim = unwords . words
