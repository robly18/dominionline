{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.IORef
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Control.Monad
import Control.Monad.Random.Lazy (lift, RandT, StdGen, runRandT, getStdGen)

import Game

app :: IORef (Log State, StdGen) -> Application
app game request respond = case rawPathInfo request of
  "/"      -> respond index
  "/send/" -> do (state, gen) <- readIORef game
                 body <- requestBody request
                 putStrLn ("Got request with body " ++ B8.unpack body)
                 case decode $ LB8.fromStrict body of
                    Nothing -> (putStrLn "Could not parse request.")>>(respond $ send $ encode state)
                    Just (plr, action) -> do
                                    let newstate = lift state >>= (flip act (plr, action)) --RandT Log State
                                    let ran = runRandT newstate gen -- Log (State, StdGen)
                                    let finalstate = fmap fst ran
                                    writeIORef game $ (finalstate, snd $ forget ran)
                                    respond $ send $ encode finalstate
  "/join/" -> do putStrLn "Joining"
                 (state,gen) <- readIORef game
                 let psp = do s <- state
                              joinGame s
                 writeIORef game $ (fmap snd psp, gen)
                 respond $ send $ encode $ fst $ forget psp
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
    gen <- getStdGen
    counter <- newIORef $ (return newGame, gen)
    run 8080 (app counter)