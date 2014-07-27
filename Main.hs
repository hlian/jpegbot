{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
-- If writing Slack bots intrigues you, check out: https://github.com/hlian/linklater
import BasePrelude hiding (words, intercalate, filter)
import Control.Lens ((^.))
import Data.Attoparsec.Text.Lazy
import Data.Char (isLetter, isAscii)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Linklater (say, slashSimple, Channel(..), Command(..), User(..), Config(..), Message(..), Icon(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

findUrl :: Text -> Maybe Text
findUrl = fmap fromStrict . maybeResult . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"'))

jpgto :: Maybe Command -> IO Text
jpgto (Just (Command (User user) channel (Just text))) = do
  message <- (fmap message . findUrl . decodeUtf8 . flip (^.) responseBody) <$> get ("http://" <> (unpack subdomain) <> ".jpg.to/")
  case (debug, message) of
    (True, _) -> putStrLn ("+ Pretending to post " <> show message) >> return ""
    (False, Just m) -> config' >>= say m >> return ""
    (False, Nothing) -> return "Something went wrong!"
  where config' = (Config "trello.slack.com" . filter (/= '\n') . pack) <$> readFile "token"
        subdomain = (intercalate "." . fmap (filter isLetter . filter isAscii) . words) text
        message url = Message (EmojiIcon "gift") channel False (mconcat ["<@", user, "|", user, ">", " <", url, "|", subdomain, ".jpg.to>"])
        debug = False
jpgto _ = return "Type more! (Did you know? jpgtobot is only 27 lines of Haskell. <https://github.com/hlian/jpgtobot/blob/master/Main.hs>)"

main :: IO ()
main = let port = 3000 in putStrLn ("+ Listening on port " <> show port) >> run port (slashSimple jpgto)
