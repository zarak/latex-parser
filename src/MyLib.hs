{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc, createCard) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.LaTeX.Base (render)
import Text.LaTeX.Base.Class (fromLaTeX)
import Text.LaTeX.Base.Parser (parseLaTeX, parseLaTeXFile)
import Text.LaTeX.Base.Syntax (LaTeX (TeXRaw), TeXArg (FixArg), lookForEnv, protectText)

-- import Text.LaTeX.Base.Pretty (prettyLaTeX)

data BasicCard = BasicCard
  { front :: Text,
    back :: Text
  }
  deriving (Show)

emptyCard :: BasicCard
emptyCard = BasicCard {front = "", back = ""}

-- Return the text for the front and back of a card
parseDefinition :: [([TeXArg], LaTeX)] -> BasicCard
parseDefinition input =
  let (FixArg def) = head (fst (input !! 1))
   in BasicCard
        { front = render def,
          back = render $ snd (input !! 1)
        }

getFirstDefinition :: LaTeX -> BasicCard
getFirstDefinition latex =
  parseDefinition $ lookForEnv "definition" latex

someFunc :: IO BasicCard
someFunc = do
  res <- parseLaTeXFile "test.tex"
  pure $ either (const emptyCard) getFirstDefinition res

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
                        "modelName" .= ("MathBasic" :: String),
                        "fields"
                          .= object
                            [ "Front" .= (T.unpack $ front card :: String),
                              "Back" .= (T.unpack $ back card :: String)
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
