{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Array.IO
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text.Format as F
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO
import           Data.Word8
import           Network.HTTP.Types (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq hiding (params)
import           Prelude hiding (readFile, writeFile, putStrLn)
import           System.Environment (getArgs)

newtype Channel = Channel Text deriving (Eq, Ord, Show)
newtype Emoji = Emoji Text
newtype User = User Text deriving (Eq, Ord)
data Notice = Notice Channel Text Emoji
data Person = Person User Channel Text

instance ToJSON Notice where
  toJSON (Notice (Channel channel) text (Emoji emoji)) =
    object [ "channel" .= T.concat ["#", channel]
           , "icon_emoji" .= T.concat [":", emoji, ":"]
           , "parse" .= String "full"
           , "username" .= String "tacobot"
           , "text" .= text
           ]

personOfRequest :: Request -> Either Text Person
personOfRequest raw = do
  channel <- channelOf <$> case p "channel_name" of
    Right "directmessage" ->
      Left "Can't randomtaco in a direct message (yet)!"
    Right "privategroup" ->
      Left "Can't randomtaco in a private group (yet)!"
    x -> x

  user <- userOf <$> p "user_name"
  text <- case p "text" of Left _ -> Right "spolsky"; x -> x
  return $ Person user channel text

  where
    params =
      M.fromList [((#<) $ k, (#<) <$> v) | (k, v) <- queryString raw]

    p key = case M.lookup key params of
      Just (Just "") ->
        Left $ T.concat ["Empty key in params: ", key]
      Just (Just value) ->
        Right value
      _  ->
        Left $ T.concat ["Unable to find key in params: ", key]

    userOf =
      User . T.filter (/= '@')

    channelOf =
      Channel

postPayload :: Text -> Notice -> IO ()
postPayload token notice = do
  args <- getArgs
  case args == ["-n"] of
    False -> do
      putStrLn (T.concat ["+ Pretending (-n) to post ", (decodeUtf8 . encode) notice])
    True -> do
      r <- post (T.unpack url) (encode notice)
      putStrLn (decodeUtf8 $ r ^. responseBody)
    where
      url = T.append "https://trello.slack.com/services/hooks/incoming-webhook?token=" token

(#<) :: B.ByteString -> Text
(#<) = decodeUtf8 . L.fromChunks . return

-- Here we go

urlParser :: Parser B.ByteString
urlParser = p
  where
    p = garbage *> url
    garbage = string "src=\"" <|> (P.take 1 *> garbage)
    url = takeTill (\c -> c == _quotedbl)

urlFor :: Text -> IO Text
urlFor search = do
  r <- get (T.unpack $ F.format "http://{}.jpg.to/" [search])
  return (handle $ parse urlParser $ strictify $ r ^. responseBody)
  where
    strictify = B.concat . L.toChunks
    handle (Fail i ctxs s) = error (show (i, ctxs, s))
    handle (Partial f) = handle (f "")
    handle (Done _ r) = (#<)$ r

application :: Application
application req respond = do
  token_ <- readFile "token"
  let token = T.filter (/= '\n') token_

  putStrLn $ T.concat [
    "+ Incoming request: "
    , (#<)$ rawPathInfo req
    , (#<)$ rawQueryString req
    ]

  case personOfRequest req of
    Right (Person (User user) channel text) -> do
      url <- urlFor text
      let response = F.format "@{} {}" (user, url)
      postPayload token (Notice channel response (Emoji "gift"))

  respondWithEmpty

  where
    ourHeaders = [("Content-Type", "text/plain")]
    respondWithEmpty = (respond . responseLBS status200 ourHeaders) ""

main :: IO ()
main = do
  let port = 83
  putStrLn (F.format "+ Listening on port {}" (F.Only port))
  run port application
  return ()
