{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8

import Game

app :: IORef State -> Application
app game request respond = case rawPathInfo request of
  "/"      -> respond index
  "/send/" -> do state <- readIORef game
                 body <- requestBody request
                 putStrLn ("Got request with body " ++ B8.unpack body)
                 case decode $ LB8.fromStrict body of
                    Nothing -> respond $ send $ encode state
                    Just (plr, action) -> do
                                    let newstate = act state (plr, action)
                                    writeIORef game newstate
                                    respond $ send $ encode newstate
  "/join/" -> do putStrLn "Joining"
                 state <- readIORef game
                 --body <- requestBody request
                 --let bodys = B8.unpack body
                 --let action = if bodys == "" then Start else (Write bodys)
                 let (plr, newstate) = joinGame state
                 writeIORef game newstate
                 respond $ send $ encode (plr, newstate)
  _        -> respond notFound


index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404"

send :: LB8.ByteString -> Response
send c = responseLBS
    status200
    [("Content-Type", "text/plain")]
    c
    

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    counter <- newIORef newGame
    run 8080 (app counter)