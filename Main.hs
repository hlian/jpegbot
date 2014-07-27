{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
-- If writing Slack bots intrigues you, check out: https://github.com/hlian/linklater
import BasePrelude hiding (words, intercalate)
import Control.Lens ((^.))
import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy hiding (filter)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Linklater (say, slashSimple, Channel(..), Command(..), User(..), Config(..), Message(..), Icon(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

findUrl :: Text -> Maybe Text
findUrl = fmap fromStrict . maybeResult . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"'))

messageOf :: User -> Channel -> Text -> Text -> Message
messageOf (User u) c search = Message (EmojiIcon "gift") c . mappend (mconcat ["@", u, " Hello, wanderer. I found you this for \"", search, "\": "])

jpgto :: Maybe Command -> IO Text
jpgto (Just (Command user channel (Just text))) = do
  message <- (fmap (messageOf user channel text) . findUrl . decodeUtf8 . flip (^.) responseBody) <$> get url
  case (debug, message) of
    (True, _) -> putStrLn ("+ Pretending to post " <> show message) >> return ""
    (False, Just m) -> config' >>= say m >> return ""
    (False, Nothing) -> return "Something went wrong!"
  where config' = (Config "trello.slack.com" . pack . filter (/= '\n')) <$> readFile "token"
        url = "http://" <> (unpack . intercalate "." . words $ text) <> ".jpg.to/"
        debug = True
jpgto Nothing = return "Type more! (Did you know? jpgtobot is only 26 lines of Haskell. <https://github.com/hlian/jpgtobot/blob/master/Main.hs>)"

main :: IO ()
main = let port = 3000 in putStrLn ("+ Listening on port " <> show port) >> run port (slashSimple jpgto)
