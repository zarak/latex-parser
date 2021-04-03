module MyLib (someFunc) where

import Text.LaTeX.Base.Parser (parseLaTeX, parseLaTeXFile)
import Text.LaTeX.Base.Syntax (lookForEnv)

someFunc :: IO ()
someFunc = do
  res <- parseLaTeXFile "test.tex"
  case res of
    Left e -> print e
    Right latex -> print $ lookForEnv "equation" latex
