module HBGen.Convert
  where

import qualified HBGen.Parser
import qualified HBGen.Markup
import qualified HBGen.Html

convertStructure :: HBGen.Markup.Structure -> HBGen.Html.Node
convertStructure structure =
    case structure of
      HBGen.Markup.Header n txt ->
          HBGen.Html.h_ n txt
      HBGen.Markup.Paragraph p ->
          HBGen.Html.p_ p
      HBGen.Markup.UnorderedList items ->
          HBGen.Html.ul_ $ map HBGen.Html.p_ items
      HBGen.Markup.OrderedList items ->
          HBGen.Html.ol_ $ map HBGen.Html.p_ items
      HBGen.Markup.CodeBlock codeLines ->
          HBGen.Html.code_ $ unlines codeLines

convert :: HBGen.Html.Title -> HBGen.Markup.Document -> HBGen.Html.Html
convert title = HBGen.Html.html_ title . foldMap convertStructure
