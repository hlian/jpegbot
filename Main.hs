{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

-- If writing Slack bots intrigues you, check out: https://github.com/hlian/linklater

import BasePrelude hiding (words, intercalate, filter)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isAlphaNum, isAscii)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Base (urlEncode)
import Network.Wai.Handler.Warp (run)

-- Naked imports.
import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy
import Network.Linklater
import Network.Wreq hiding (params)

findUrl :: ByteString -> Maybe Text
findUrl =
  encode . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"')) . decode
  where
    encode =
      fmap fromStrict . maybeResult
    decode =
      decodeUtf8

configIO :: IO Config
configIO = do
  token <- (filter (/= '\n') . pack) <$> readFile "token"
  return $ Config "trello.slack.com" token

urlOf :: Text -> Text -> String
urlOf query options =
  "http://" <> urlEncode (unpack query) <> ".jpg.to/" <> unpack options

parseText :: Text -> Maybe (Text, Text)
parseText text =
  f (strip <$> (splitOn "--" text))
  where
    f []             = mzero
    f [raw]          = return (parseRaw raw, "")
    f [raw, options] = return (parseRaw raw, options)
    f _              = mzero
    parseRaw         = intercalate "." . words

liftMaybe :: Maybe a -> MaybeT IO a
liftMaybe = maybe mzero return

messageOfCommand :: Command -> MaybeT IO Message
messageOfCommand (Command _ _ (Nothing)) =
  mzero
messageOfCommand (Command user channel (Just text)) = do
  (query, options) <- liftMaybe $ parseText text
  response <- liftIO $ get (urlOf query options)
  let formatsOf url = [FormatAt user, FormatLink url (query <> ".jpg.to/" <> options)]
  liftMaybe $ (messageOf . formatsOf) <$> findUrl (response ^. responseBody)
  where
    messageOf =
      FormattedMessage (EmojiIcon "gift") "jpgtobot" channel

jpgto :: Maybe Command -> IO Text
jpgto Nothing = do
  return "Unrecognized Slack request!"

jpgto (Just command) = do
  putStrLn ("+ Incoming command: " <> show command)
  config <- configIO
  message <- (runMaybeT . messageOfCommand) command
  case (debug, message) of
    (True, _) -> do
      putStrLn ("+ Outgoing messsage: " <> show (encode <$> message))
      return ""
    (False, Just m) -> do
      say m config
      return ""
    (False, Nothing) ->
      return "*FRIZZLE* ERROR PROCESSING INPUT; BEGIN SELF-DETONATION; PLEASE FILE ISSUE AT <https://github.com/hlian/jpgtobot>"
  where
    debug = False

main :: IO ()
main = do
  putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple jpgto)
    where
      port = 3333
