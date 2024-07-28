{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import Network.Wai
-- import Network.HTTP.Types
-- import Network.Wai.Handler.Warp (run)
import Audio


main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    playWhiteNoise
    -- run 8080 app
