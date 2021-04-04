module TelegramAPI.Methods where

import TelegramAPI.Types
import Network.HTTP.Simple
import GHC.Generics
import Data.Maybe
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BS
import Data.Aeson
import Data.Text 
import qualified Data.ByteString.Lazy.Char8 as LBC

telegramHost :: BS.ByteString
telegramHost = "api.telegram.org"

tokens :: BS.ByteString
tokens = "1709255971:AAE0WBzFHg0AKXAUK0b93xKHGNjfkU5Q6FE"

telegramPath :: BS.ByteString
telegramPath = "bot" <> tokens


getUpdates = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/getUpdates")
                     $ defaultRequest

getUpdatesWithOffset upid = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/getUpdates")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("offset", Just $ BS.pack $ show $ upid)]

echoMessage m = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/copyMessage")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ ctId m))
                   , ("from_chat_id", Just $ BS.pack (show $ ctId m))
                   , ("message_id", Just $ BS.pack (show $ mesId m))]

sendMessage m = setRequestHost telegramHost
            $ setRequestPath (telegramPath <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ ctId m))
                   , ("text", Just $ BS.pack "/help - command with help.\n/stop - command stop bot\n")]

keyboard m = setRequestHost telegramHost
            $ setRequestPath (telegramPath <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ ctId m))
                   , ("text", Just $ BS.pack $ "Select the number of\nrepetitions of your message")
                   , ("reply_markup", Just $ BS.pack $ LBC.unpack $ encode inlineKeyboard)]
          keyboardButton = [ InlineKeyboardButton "1" (Just "1")
                           , InlineKeyboardButton "2" (Just "2")
                           , InlineKeyboardButton "3" (Just "3")
                           , InlineKeyboardButton "4" (Just "4")
                           , InlineKeyboardButton "5" (Just "5")]
          inlineKeyboard =  InlineKeyboardMarkup [keyboardButton]

ctId upd = user_id $ fromJust upd
fromId upd = from_id $ fromJust upd
mesId upd = message_id $ fromJust upd
textT upd = text $ fromJust upd
