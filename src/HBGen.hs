module HBGen 
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import System.IO ( Handle, hGetContents, hPutStrLn )
import HBGen.Convert ( convert )
import HBGen.Html.Internal ( Title, render )
import HBGen.Parser ( parse )

convertSingle :: Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO()
convertDirectory = error "Not implemented yet."


process :: Title -> String -> String
process title = render . convert title . parse
