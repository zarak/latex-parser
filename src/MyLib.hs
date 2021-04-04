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
          [ "action" .= ("deckNames" :: String)
          , "version" .= (6 :: Int)
          ]

    r <-
      req
      POST
      (http "localhost")
      (ReqBodyJson payload)
      jsonResponse
      (port 8765)
    liftIO $ print (responseBody r :: Value)

createCard :: IO ()
createCard = runReq defaultHttpConfig $ do
    let payload =
          object
          [ "action" .= ("addNote" :: String)
          , "version" .= (6 :: Int)
          , "params" .= object
                        [ "note" .= object
                                    [ "deckName" .= ("All" :: String)
                                    , "modelName" .= ("Basic" :: String)
                                    , "fields" .= object
                                                  [ "Front" .= ("front content" :: String)
                                                  , "Back" .= ("back content" :: String)
                                                  ]
                                    ]
                        ]
          ]

    r <-
      req
      POST
      (http "localhost")
      (ReqBodyJson payload)
      jsonResponse
      (port 8765)
    liftIO $ print (responseBody r :: Value)
