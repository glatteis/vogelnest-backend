module Main where

import           RestAPI
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 restAPI
