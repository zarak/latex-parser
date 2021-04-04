{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.LaTeX.Base (render)
import Text.LaTeX.Base.Parser (parseLaTeX, parseLaTeXFile)
import Text.LaTeX.Base.Syntax (LaTeX (TeXRaw), TeXArg (FixArg), lookForEnv)

data BasicCard = BasicCard
  { front :: String
  , back :: String
  }

-- Return the text for the front and back of a card
parseDefinition :: [([TeXArg], LaTeX)] -> (Text, Text)
parseDefinition input = do
  let (FixArg def) = head (fst (head input))
  (render def, render $ snd (head input))

someFunc :: IO ()
someFunc = do
  res <- parseLaTeXFile "test.tex"
  case res of
    Left e -> print e
    Right latex -> do
      -- print $ lookForEnv "equation" latex
      let x = parseDefinition $ lookForEnv "definition" latex
      print x

getDeckNames :: IO ()
getDeckNames = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "action" .= ("deckNames" :: String),
            "version" .= (6 :: Int)
          ]

  r <-
    req
      POST
      (http "localhost")
      (ReqBodyJson payload)
      jsonResponse
      (port 8765)
  liftIO $ print (responseBody r :: Value)

createCard :: BasicCard -> IO ()
createCard card = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "action" .= ("addNote" :: String),
            "version" .= (6 :: Int),
            "params"
              .= object
                [ "note"
                    .= object
                      [ "deckName" .= ("Test" :: String),
                        "modelName" .= ("Basic" :: String),
                        "fields"
                          .= object
                            [ "Front" .= (front card :: String),
                              "Back" .= (back card :: String)
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
