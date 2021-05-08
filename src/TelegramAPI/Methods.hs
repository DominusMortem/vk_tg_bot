module TelegramAPI.Methods 
    ( getUpdates
    , getUpdatesWithOffset
    , echoMessage
    , sendMessage
    , keyboard
    , echoM_
    , helpM
    , repeatM
    , callB
    )where

import TelegramAPI.Types
import Network.HTTP.Simple
import Config
import Data.Maybe                           (catMaybes)
import Data.Aeson                           (encode)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map                   as M

telegramHost :: BS.ByteString
telegramHost = "api.telegram.org"

getUpdates :: Config -> Request
getUpdates cfg = setRequestHost telegramHost
                     $ setRequestPath ("bot" <> BS.pack (token cfg) <> "/getUpdates")
                     $ setRequestQueryString [(
                        "timeout", Just $ BS.pack $ show $ timeout cfg)]
                     $ defaultRequest

getUpdatesWithOffset :: Config -> Int -> Request
getUpdatesWithOffset cfg upid = setRequestHost telegramHost
                     $ setRequestPath ("bot" <> BS.pack (token cfg) <> "/getUpdates")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("offset", Just $ BS.pack $ show $ upid)
                   , ("timeout", Just $ BS.pack $ show $ timeout cfg)]

echoMessage :: Config -> Message -> Request
echoMessage cfg m = setRequestHost telegramHost
                     $ setRequestPath ("bot" <> BS.pack (token cfg) <> "/copyMessage")
                     $ setRequestQueryString params
                     $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ chat_id m))
                   , ("from_chat_id", Just $ BS.pack (show $ chat_id m))
                   , ("message_id", Just $ BS.pack (show $ message_id m))]

sendMessage :: Config -> Message -> Request
sendMessage cfg m = setRequestHost telegramHost
            $ setRequestPath ("bot" <> BS.pack (token cfg) <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    where params = [ ("chat_id", Just $ BS.pack (show $ chat_id m))
                   , ("text", Just $ BS.pack $ messagehelp cfg)]

keyboard :: Config -> M.Map Int Int -> Message -> Request
keyboard cfg dict m = setRequestHost telegramHost
            $ setRequestPath ("bot" <> BS.pack (token cfg) <> "/sendMessage")
            $ setRequestQueryString params
            $ defaultRequest
    
    where params = [ ("chat_id", Just $ BS.pack (show $ chat_id m))
                   , ("text", Just $ BS.pack $ forRepeat cfg)
                   , ("reply_markup", Just $ BS.pack $ LBC.unpack $ encode inlineKeyboard)]
          
          keyboardButton = [ InlineKeyboardButton "1" (Just "1")
                           , InlineKeyboardButton "2" (Just "2")
                           , InlineKeyboardButton "3" (Just "3")
                           , InlineKeyboardButton "4" (Just "4")
                           , InlineKeyboardButton "5" (Just "5")]
          
          inlineKeyboard =  InlineKeyboardMarkup [keyboardButton]
          
          forRepeat x = concat ["Current number of repetitions "
                               , show $ numrepeat x
                               ,"\n"
                               , messagerepeat x]
          numrepeat x = M.findWithDefault (repeats x) (from_id m) dict

echoM_ :: [Update] -> Int -> M.Map Int Int -> [Message]
echoM_ upd rep dict = concat (map repeats (updates upd))
    where
        updates u = filter (\x -> text x /= (Just "/help") && 
                                  text x /= (Just "/repeat")) 
                           (catMaybes $ map message u)
        repeats x = take (newRepeat x) $ repeat x
        newRepeat x = M.findWithDefault (rep)
                      (from_id x) dict
helpM :: [Update] -> [Message]
helpM upd = filter (\x -> text x == (Just "/help")) 
                   (catMaybes $ map message upd)

repeatM :: [Update] -> [Message]
repeatM upd = filter (\x -> text x == (Just "/repeat"))
                     (catMaybes $ map message upd)

callB :: [Update] -> [CallbackQuery]
callB upd = (catMaybes $ map callback_query upd)
