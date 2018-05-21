module LibMain where

import Server
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 application
