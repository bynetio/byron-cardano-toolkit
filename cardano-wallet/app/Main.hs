module Main where

import           NodeCli (cliApp, runApp)

main :: IO ()
main = do
  putStrLn "Starting server on port 8081..."
  runApp cliApp 8081
