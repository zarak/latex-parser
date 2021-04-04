module Params
  ( Params
  , cmdLineParser
  ) where

import Options.Applicative (Parser, strArgument, metavar, help, execParser, info, (<**>), helper, fullDesc, progDesc)

data Params = Params
  { fname :: FilePath
  }

mkParams :: Parser Params
mkParams =
  Params <$>
    strArgument
      (metavar "FILE" <> help "Tex file name")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Create Anki cards from tex files")
