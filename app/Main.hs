{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Audio (test)

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    test
    -- run 8080 app
