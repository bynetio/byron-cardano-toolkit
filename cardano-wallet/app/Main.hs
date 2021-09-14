module Main where

import HttpEndpoints (app)
import Network.Wai ( Application )
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn "Starting server on port 8081..."
  runApp app 8081

runApp :: Application -> Int -> IO ()
runApp app port = run port app