{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- If writing Slack bots intrigues you, check out: https://github.com/hlian/linklater

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Char (isAlphaNum, isAscii)
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Base (urlEncode)
import           Network.Images.Search as Search
import           Network.Wai.Handler.Warp (run)

-- Naked imports.
import           BasePrelude hiding (words, intercalate)
import           Data.Attoparsec.Text.Lazy
import           Network.Linklater
import           Network.Wreq hiding (params)

cleverlyReadFile filename =
  T.filter (/= '\n') . T.pack <$> readFile filename

configIO :: IO Config
configIO =
  Config <$> (cleverlyReadFile "hook")

googleConfigIO :: IO Search.Gapi
googleConfigIO =
  Search.config <$> (cleverlyReadFile "google-server-key") <*> (cleverlyReadFile "google-search-engine-id")

parseText :: Text -> Maybe Text
parseText text = case T.strip text of
  "" -> Nothing
  x -> Just x

liftMaybe :: Maybe a -> MaybeT IO a
liftMaybe = maybe mzero return

messageOfCommand :: Command -> MaybeT IO Message

messageOfCommand (Command "jpeg" _ _ Nothing) =
  mzero
messageOfCommand (Command "jpeg" user channel (Just text)) = do
  gapi <- liftIO googleConfigIO
  query <- liftMaybe (parseText text)
  urls <- liftIO (Search.linksOfQuery gapi query)
  url <- liftMaybe (listToMaybe urls)
  return (messageOf [FormatAt user, FormatLink url url])
  where
    messageOf =
      FormattedMessage (EmojiIcon "gift") "jpgtobot" channel

jpgto :: Maybe Command -> IO Text
jpgto Nothing =
  return "Unrecognized Slack request!"

jpgto (Just command) = do
  putStrLn ("+ Incoming command: " <> show command)
  config <- configIO
  message <- (runMaybeT . messageOfCommand) command
  putStrLn ("+ Outgoing messsage: " <> show (encode <$> message))
  case (debug, message) of
    (False, Just m) -> do
      say m config
      return ""
    (False, Nothing) ->
      return "*FRIZZLE* ERROR PROCESSING INPUT; BEGIN SELF-DETONATION; PLEASE FILE ISSUE AT <https://github.com/hlian/jpgtobot>"
    _ ->
      return ""
  where
    debug = False

main :: IO ()
main = do
  putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple jpgto)
    where
      port = 3333
