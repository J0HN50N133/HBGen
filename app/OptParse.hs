-- | Command-line options parsing
module OptParse
where

import Data.Maybe
import Options.Applicative

--------------------------------------------------
-- * command-line options model

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

-- | A single input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

-- | A single output source
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

--------------------------------------------------
-- * Parser

-- | Parse cmd-line options

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
    info (pOptions <**> helper)
    (fullDesc
        <> header "HBGen - a static blog generator"
        <> progDesc "Yet another generator converts markup files or directories to html"
    )

-- | Parser for all options
pOptions :: Parser Options
pOptions =
    subparser
        ( command
            "convert"
            ( info
              (helper <*> pConvertSingle)
              (progDesc "Convert a single markup source to html")
            )
        <>command
            "convert-dir"
            ( info
                (helper <*> pConvertDir)
                (progDesc "Convert a directory of markup files to html")
            )
        )

--------------------------------------------------
-- * Single source to sink conversion parser

-- | Parser for single source to sink option
pConvertSingle :: Parser Options
pConvertSingle =
    liftA2 ConvertSingle pSingleInput pSingleOutput

-- | Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
    fromMaybe Stdin <$> optional pInputFile

-- | Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
    fromMaybe Stdout <$> optional pOutputFile

-- | Input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
    where
        parser =
            strOption
                ( long "input"
                  <> short 'i'
                  <> metavar "FILE"
                  <> help "Input file"
                )

-- | Input file parser
pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
    where
        parser =
            strOption
                ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> help "Output file"
                )


--------------------------------------------------
-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir =
    liftA2 ConvertDir pInputDir pOutputDir

-- | Input Directory parser
pInputDir :: Parser FilePath
pInputDir =
    strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

-- | Output Directory parser
pOutputDir :: Parser FilePath
pOutputDir =
    strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )
