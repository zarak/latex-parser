module Main where

import Params (Params, cmdLineParser)
import MyLib (someFunc, sendToAnki)

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  fd <- someFunc
  sendToAnki fd

