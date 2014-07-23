{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Format as F
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy.IO
import           Data.Word8
import           Network.HTTP.Types (status200)
import           Network.Linklater
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq hiding (params)
import           Prelude hiding (readFile, writeFile, putStrLn)
import           System.Environment (getArgs)

toText :: B.ByteString -> Text
toText = decodeUtf8 . L.fromChunks . return

urlParser :: Parser B.ByteString
urlParser = p
  where
    p = garbage *> url
    garbage = string "src=\"" <|> (P.take 1 *> garbage)
    url = takeTill (== _quotedbl)

urlFor :: Text -> IO Text
urlFor search = do
  r <- get (T.unpack $ F.format "http://{}.jpg.to/" [search])
  (return . handle . parse urlParser . strictly) (r ^. responseBody)
  where
    strictly = B.concat . L.toChunks
    handle (Fail i ctxs s) = error (show (i, ctxs, s))
    handle (Partial f) = handle (f "")
    handle (Done _ r) = toText r

jpgto :: Maybe Command -> Application
jpgto (Just (Command (User user) channel text)) req respond = do
  args <- getArgs
  token <- T.filter (/= '\n') <$> readFile "token"
  putStrLn debugString
  url <- urlFor (maybe "spolsky" id text)
  let response = F.format "@{} {}" (user, url)
  let config = Config token "trello.slack.com"
  case args of
    ["-n"] -> putStrLn (F.format "+ Pretending to post {}" [response])
    _ -> void $ say (Message channel response (EmojiIcon "gift")) config
  respondWithEmpty

  where
    debugString = T.concat [
      "+ Incoming request: "
      , toText $ rawPathInfo req
      , toText $ rawQueryString req
      ]
    ourHeaders = [("Content-Type", "text/plain")]
    respondWithEmpty = (respond . responseLBS status200 ourHeaders) ""

main :: IO ()
main = do
  let port = 83
  putStrLn (F.format "+ Listening on port {}" (F.Only port))
  run port (slash jpgto)
  return ()
