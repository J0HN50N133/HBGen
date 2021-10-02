module Parser
  ( parse )
  where
import Numeric.Natural
import Data.Maybe
import Markup

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
        case txts of
          -- end
          [] -> maybeToList context

          -- H1
          ('*': ' ' : line): rest ->
            maybe id (:) context $ Header 1 (trim line) : parseLines Nothing rest
          -- H2
          ('*': '*': ' ' : line): rest ->
            maybe id (:) context $ Header 2 (trim line) : parseLines Nothing rest
          -- H3
          ('*': '*' : '*' : ' ' : line): rest ->
            maybe id (:) context $ Header 3 (trim line) : parseLines Nothing rest
          -- H4
          ('*': '*' : '*' : '*' : ' ' : line): rest ->
            maybe id (:) context $ Header 4 (trim line) : parseLines Nothing rest
          -- H5
          ('*': '*' : '*' : '*' : '*' : ' ' : line): rest ->
            maybe id (:) context $ Header 5 (trim line) : parseLines Nothing rest
          -- UnorderedList list
          ('-': ' ': line): rest ->
            case context of
              Just (UnorderedList list) ->
                parseLines (Just (UnorderedList (list <> [trim line]))) rest
              _ ->
                maybe id (:) context $ parseLines (Just (UnorderedList [trim line])) rest
          -- OrdereredList
          ('#': ' ': line): rest ->
            case context of
              Just (OrderedList list) ->
                parseLines (Just (OrderedList (list <> [trim line]))) rest
              _ ->
                maybe id (:) context $ parseLines (Just (OrderedList [trim line])) rest
          -- Code Block
          ('>' : ' ': line): rest ->
            case context of
              Just (CodeBlock list) ->
                parseLines (Just (CodeBlock (list <> [line]))) rest
              _ ->
                maybe id (:) context $ parseLines (Just (CodeBlock [line])) rest
          -- paragraph
          currentLine : rest ->
            let
              line = trim currentLine
            in
              if line == ""
                 then
                    maybe id (:) context $ parseLines Nothing rest
                 else
                    case context of
                      Just (Paragraph p) -> 
                        -- unwords here is extremely slow
                        -- TODO: add efficient context data
                        parseLines (Just (Paragraph (unwords [p, line]))) rest
                      _ ->
                        maybe id (:) context $ parseLines (Just (Paragraph line)) rest


trim :: String -> String
trim = unwords . words
