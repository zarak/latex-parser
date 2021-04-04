module Main where

import MyLib (sendToAnki, someFunc)
import Params (Params, cmdLineParser)

main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  fd <- someFunc
  sendToAnki fd
