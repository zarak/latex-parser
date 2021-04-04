module Main where

import Params (Params, cmdLineParser)
import MyLib (someFunc, createCard)

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  fd <- someFunc
  createCard fd

