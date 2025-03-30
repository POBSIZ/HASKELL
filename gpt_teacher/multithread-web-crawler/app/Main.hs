{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Data.Default.Class (def)
import Data.Time.Clock
import Lib (urls)
import Network.Connection
  ( TLSSettings (..),
  )
import Network.HTTP.Client
  ( Manager,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import Network.HTTP.Client.TLS (mkManagerSettings, tlsManagerSettings)
import Text.HTML.TagSoup (fromAttrib, isTagOpenName, parseTags)
import Text.StringLike (StringLike)

timeIt :: IO a -> IO a
timeIt action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  putStrLn $ "⏱  Elapsed time: " ++ show (diffUTCTime end start)
  return result

-- Custom TLS settings to allow non-EMS connections
customTLSSettings :: TLSSettings
customTLSSettings =
  TLSSettingsSimple
    { settingDisableCertificateValidation = False,
      settingDisableSession = False,
      settingUseServerName = True,
      settingClientSupported = def
    }

-- Create a manager with relaxed EMS requirements
managerWithRelaxedEMS :: IO Manager
managerWithRelaxedEMS = newManager $ mkManagerSettings customTLSSettings Nothing

-- Fetch a URL with the custom manager
fetchURL :: Manager -> String -> IO (Either String String)
fetchURL manager url =
  do
    request <- parseRequest url
    response <- httpLbs request manager
    return $ Right (show $ responseBody response)
    `catch` \e -> return $ Left (show (e :: SomeException))

asyncFetchData :: Manager -> Manager -> String -> IO String
asyncFetchData defaultManager relaxedManager url = do
  resultDefault <- fetchURL defaultManager url
  resultRelaxed <- fetchURL relaxedManager url

  case resultDefault of
    Right response -> return response
    Left _ -> case resultRelaxed of
      Right response -> return response
      Left err2 -> return $ "Error: " ++ show err2

extractLinks :: (Show p, StringLike p) => p -> [p]
extractLinks body = links
  where
    tags = parseTags body
    links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

getLinks :: [String] -> IO [[String]]
getLinks _urls = timeIt $ do
  defaultManager <- newManager tlsManagerSettings
  relaxedManager <- managerWithRelaxedEMS
  responses <- mapConcurrently (asyncFetchData defaultManager relaxedManager) _urls
  return $ map extractLinks responses

main :: IO [[String]]
main = do
  getLinks urls