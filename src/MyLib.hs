{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc, sendToAnki) where

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

data MathBasic = MathBasic
  { front :: Text,
    back :: Text
  }
  deriving (Show)

data MathCloze = MathCloze
  { clozeText :: Text,
    extra :: Text
  }
  deriving (Show)

-- Return the text for the front and back of a card
parseDefinition :: ([TeXArg], LaTeX) -> MathBasic
parseDefinition ([], latex) =
  MathBasic
    { front = "",
      back = render latex
    }
parseDefinition (firstArg : restArgs, latex) =
  let (FixArg def) = firstArg
   in MathBasic
        { front = render def,
          back = render latex
        }

getDefinitions :: LaTeX -> [MathBasic]
getDefinitions latex =
  parseDefinition <$> lookForEnv "definition" latex

someFunc :: IO [MathBasic]
someFunc = do
  res <- parseLaTeXFile "test.tex"
  pure $ either (const []) getDefinitions res

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

sendToAnki :: [MathBasic] -> IO ()
sendToAnki cards = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "action" .= ("addNotes" :: String),
            "version" .= (6 :: Int),
            "params"
              .= object
                [ "notes" .= cardObjects cards ]
          ]

  r <-
    req
      POST
      (http "localhost")
      (ReqBodyJson payload)
      jsonResponse
      (port 8765)
  liftIO $ print (responseBody r :: Value)


cardObjects :: [MathBasic] -> [Value]
cardObjects =
  map f 
    where
      f = \c ->
              object
                [ "deckName" .= ("Test" :: String),
                  "modelName" .= ("MathBasic" :: String),
                  "fields"
                    .= object
                      [ "Front" .= (T.unpack $ front c :: String),
                        "Back" .= (T.unpack $ back c :: String)
                      ]
                ]
