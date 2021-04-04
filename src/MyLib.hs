{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where

import Text.LaTeX.Base.Parser (parseLaTeX, parseLaTeXFile)
import Text.LaTeX.Base.Syntax (lookForEnv)
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)

someFunc :: IO ()
someFunc = do
  res <- parseLaTeXFile "test.tex"
  case res of
    Left e -> print e
    Right latex -> print $ lookForEnv "equation" latex


getDeckNames :: IO ()
getDeckNames = runReq defaultHttpConfig $ do
    let payload =
          object
          [ "action" .= ("decknames" :: String)
          , "version" .= (6 :: Int)
          ]

    r <-
      req
      POST
      (http "localhost:8765")
      (ReqBodyJson payload)
      jsonResponse
      mempty
    liftIO $ print (responseBody r :: Value)

