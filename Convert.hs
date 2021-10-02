module Convert where

import qualified Parser
import qualified Markup
import qualified Html

convertStructure :: Markup.Structure -> Html.Node
convertStructure structure =
    case structure of
      Markup.Header n txt ->
          Html.h_ n txt
      Markup.Paragraph p ->
          Html.p_ p
      Markup.UnorderedList items ->
          Html.ul_ $ map Html.p_ items
      Markup.OrderedList items ->
          Html.ol_ $ map Html.p_ items
      Markup.CodeBlock codeLines ->
          Html.code_ $ unlines codeLines

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

process :: Html.Title -> String -> String
process title = Html.render . convert title . Parser.parse
