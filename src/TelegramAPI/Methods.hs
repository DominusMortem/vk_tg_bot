module TelegramAPI.Methods where

import TelegramAPI.Types
import Network.HTTP.Simple
import Data.Maybe                           (catMaybes)
import Data.Aeson                           (encode)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map                   as M

telegramHost :: BS.ByteString
telegramHost = "api.telegram.org"

tokens :: BS.ByteString
tokens = "1709255971:AAE0WBzFHg0AKXAUK0b93xKHGNjfkU5Q6FE"

telegramPath :: BS.ByteString
telegramPath = "bot" <> tokens

getUpdates :: Request
getUpdates = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/getUpdates")
                     $ defaultRequest

getUpdatesWithOffset :: Int -> Request
getUpdatesWithOffset upid = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/getUpdates")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("offset", Just $ BS.pack $ show $ upid)]

echoMessage :: Message -> Request
echoMessage m = setRequestHost telegramHost
                     $ setRequestPath (telegramPath <> "/copyMessage")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ user_id m))
                   , ("from_chat_id", Just $ BS.pack (show $ user_id m))
                   , ("message_id", Just $ BS.pack (show $ message_id m))]

sendMessage :: Message -> Request
sendMessage m = setRequestHost telegramHost
            $ setRequestPath (telegramPath <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ user_id m))
                   , ("text", Just $ BS.pack "/help - command with help.\n/stop - command stop bot\n")]

keyboard :: Message -> Request
keyboard m = setRequestHost telegramHost
            $ setRequestPath (telegramPath <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ user_id m))
                   , ("text", Just $ BS.pack $ "Select the number of\nrepetitions of your message")
                   , ("reply_markup", Just $ BS.pack $ LBC.unpack $ encode inlineKeyboard)]
          keyboardButton = [ InlineKeyboardButton "1" (Just "1")
                           , InlineKeyboardButton "2" (Just "2")
                           , InlineKeyboardButton "3" (Just "3")
                           , InlineKeyboardButton "4" (Just "4")
                           , InlineKeyboardButton "5" (Just "5")]
          inlineKeyboard =  InlineKeyboardMarkup [keyboardButton]

echoM_ :: [Update] -> String -> M.Map Int String -> [Message]
echoM_ upd rep dict = Prelude.concat (Prelude.map repeats (updates upd))
    where
        updates u = Prelude.filter (\x -> text x /= (Just "/help") && 
                                          text x /= (Just "/repeat")) 
                                   (catMaybes $ Prelude.map message u)
        repeats x = Prelude.take (read $ newRepeat x :: Int) $ Prelude.repeat x
        newRepeat x = M.findWithDefault (rep)
                                      (from_id x) dict
helpM :: [Update] -> [Message]
helpM upd = Prelude.filter (\x -> text x == (Just "/help")) (catMaybes $ Prelude.map message upd)

repeatM :: [Update] -> [Message]
repeatM upd = Prelude.filter (\x -> text x == (Just "/repeat")) (catMaybes $ Prelude.map message upd)

callB :: [Update] -> [CallbackQuery]
callB upd = (catMaybes $ Prelude.map callback_query upd)
