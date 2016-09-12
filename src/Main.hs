{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.CaseInsensitive (original)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import System.Environment (getArgs)
import System.Console.ANSI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8


main :: IO ()
main = do
    args <- getArgs
    port <- case args of
        [port] -> return $ fromMaybe (error "Argument must be a number.")
                                     (readMaybe port)
        _ -> return 3000
    putStrLn $ "starting on port " ++ show port
    run port $ app


app :: Application
app req res = do

    setSGR [SetColor Foreground Vivid Red]
    B8.putStr $ requestMethod req
    setSGR [Reset]
    B8.putStr " "
    putStrLn . show $ httpVersion req

    setSGR [SetColor Foreground Dull White]
    putStr "URL: "
    setSGR [Reset]
    B8.putStrLn $ rawPathInfo req

    setSGR [SetColor Foreground Dull White]
    putStr "Query: "
    setSGR [Reset]
    B8.putStrLn $ rawQueryString req

    forM_ (requestHeaders req) $ \(h, v) -> do
        setSGR [SetColor Foreground Dull White]
        B8.putStr $ original h
        putStr ": "
        setSGR [Reset]
        B8.putStrLn v

    setSGR [Reset]
    printBodyChunks

    putStrLn ""

    res $ responseLBS status200
        [("Content-Type", "text/plain")]
        ""
  where
    printBodyChunks = do
        c <- requestBody req
        B8.putStr c
        if BS.null c
            then return ()
            else printBodyChunks
