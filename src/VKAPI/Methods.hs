module VKAPI.Methods 
    ( getLongPollServer
    , getLongPollAPI
    , echoMessage
    , sendMessage
    , keyboard
    , payloadM
    , echoM
    , repeatM
    , helpM
    )where

import VKAPI.Types
import Config
import Network.HTTP.Simple
import Data.Maybe                                   (catMaybes)
import Data.Aeson                                   (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

ver_api :: BC.ByteString
ver_api = "5.130"

apiHost :: BC.ByteString
apiHost = "api.vk.com"

getLongPollServer :: Config -> Request
getLongPollServer cfg = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/groups.getLongPollServer"
                     $ defaultRequest
    where params = [ ("group_id", Just $ BC.pack (show $ group_id cfg))
                   , ("access_token", Just $ BC.pack $ token cfg)
                   , ("v", Just $ ver_api)]

getLongPollAPI :: InitValue -> Config -> Request
getLongPollAPI initial conf = setRequestQueryString params
                     $ parseRequest_ $ server initial
    where params = [ ("act", Just $ BC.pack ("a_check"))
                   , ("key", Just $ BC.pack $ key initial)
                   , ("wait", Just $ BC.pack $ show $ timeout conf)
                   , ("ts", Just $ BC.pack $ ts_s initial)]

echoMessage :: Config -> Message -> Request
echoMessage cfg upd = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where params = [ ("access_token", Just $ BC.pack $ token cfg)
                   , ("user_id", Just $ BC.pack (show $ from_id upd))
                   , ("random_id", Just $ BC.pack (show $ random_id upd))
                   , ("forward_messages", Just $ BC.pack (show $ id_mes upd))
                   , ("v", Just $ ver_api)]

sendMessage :: Config -> Message -> Request
sendMessage cfg upd = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where params = [ ("user_id", Just $ BC.pack (show $ from_id upd))
                   , ("random_id", Just $ BC.pack (show $ random_id upd))
                   , ("access_token", Just $ BC.pack $ token cfg)
                   , ("message", Just $ BC.pack $ messagehelp cfg)
                   , ("v", Just ver_api)]         

keyboard :: Config -> M.Map Int Int -> Message -> Request
keyboard cfg dict upd = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where params = [ ("user_id",      Just $ BC.pack (show $ from_id upd))
                   , ("random_id",    Just $ BC.pack (show $ random_id upd)) 
                   , ("message",      Just $ BC.pack $ forRepeat cfg)
                   , ("keyboard",     Just $ BC.pack $ LBC.unpack $ encode inlineKeyboard)
                   , ("access_token", Just $ BC.pack $ token cfg)
                   , ("v",            Just ver_api)]
          keyboardButton = [ Buttons (Action "text" "1" "1") "primary"
                           , Buttons (Action "text" "2" "2") "primary"
                           , Buttons (Action "text" "3" "3") "primary"
                           , Buttons (Action "text" "4" "4") "primary"
                           , Buttons (Action "text" "5" "5") "primary"]
          inlineKeyboard =  Keyboard  False [keyboardButton] True 

          forRepeat x = concat ["Current number of repetitions "
                               , show $ numrepeat x
                               ,"\n"
                               , messagerepeat x]
          numrepeat x = M.findWithDefault (repeats x) (from_id upd) dict


payloadM :: [Update] -> [Message]
payloadM upd = filter (\x -> payload x /= Nothing) (catMaybes $ map (message . object_u) upd)


echoM :: [Update] -> Int -> M.Map Int Int -> [Message]
echoM upd rep dict = concat (map repeats (updates upd))
    where
        updates u = filter (\x -> text x /= "/help" && 
                                          text x /= "/repeat" &&
                                          payload x == Nothing)
                                   (catMaybes $ map (message . object_u) u)
        repeats x = take (newRepeat x) $ repeat x
        newRepeat x = M.findWithDefault (rep)
                                      (from_id x) dict


repeatM :: [Update] -> [Message]
repeatM upd = filter (\x -> text x == "/repeat") (catMaybes $ map (message . object_u) upd)

helpM :: [Update] -> [Message]
helpM upd = filter (\x -> text x == "/help") (catMaybes $ map (message . object_u) upd)

