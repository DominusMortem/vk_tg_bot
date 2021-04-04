module VKAPI.Methods where

import VKAPI.Types
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
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as LBC

vktoken :: String
vktoken = "a760727e1d2397f34e294a6d5ea7a76f29ad80ffd4cde8ebcad71586b51ec071f9c74fc57416c41a62e51"

group_id :: Int
group_id = 203655745

ver_api :: BS.ByteString
ver_api = "5.130"

apiHost :: BS.ByteString
apiHost = "api.vk.com"

getLongPollServer = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/groups.getLongPollServer"
                     $ defaultRequest
    where params = [ ("group_id", Just $ BS.pack (show $ group_id))
                   , ("access_token", Just $ BS.pack vktoken)
                   , ("v", Just $ ver_api)]

getLongPollAPI sv key_ tS = setRequestQueryString params
                     $ parseRequest_ sv
    where params = [ ("act", Just $ BS.pack ("a_check"))
                   , ("key", Just $ BS.pack key_)
                   --, ("wait", Just $ BS.pack ("25"))
                   , ("ts", Just $ BS.pack (show $ tS))]

echoMessage idmes peer rand = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where params = [ ("user_id", Just $ BS.pack (show $ peer))
                   , ("random_id", Just $ BS.pack (show $rand))
                   , ("forward_messages", Just $ BS.pack (show $ idmes))
                   , ("access_token", Just $ BS.pack vktoken)
                   , ("v", Just $ ver_api)]

sendMessage peer rand = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where params = [ ("user_id", Just $ BS.pack (show $ peer))
                   , ("random_id", Just $ BS.pack (show $ rand))
                   , ("access_token", Just $ BS.pack vktoken)
                   , ("message", Just $ BS.pack "/help - for help\n/repeat - for repeat")
                   , ("v", Just ver_api)]

keyboard peer rand = setRequestHost apiHost
                     $ setRequestQueryString params
                     $ setRequestPath "/method/messages.send"
                     $ defaultRequest
    where
      params = [ ("user_id",      Just $ BS.pack (show $ peer))
           , ("random_id",    Just $ BS.pack (show $ rand)) 
           , ("message",      Just $ BS.pack $ "Select the number of\nrepetitions of your message")
           , ("keyboard",     Just $ BS.pack $ LBC.unpack $ encode keyboards)
           , ("access_token", Just $ BS.pack vktoken)
           , ("v",            Just ver_api)]
      keyboardButton = [ Buttons (Action "text" "1" "1") "primary"
                      , Buttons (Action "text" "2" "2") "primary"
                      , Buttons (Action "text" "3" "3") "primary"
                      , Buttons (Action "text" "4" "4") "primary"
                      , Buttons (Action "text" "5" "5") "primary"]
      inlineKeyboard =  Keyboard  False [keyboardButton] True 

vkKey sv = key sv
vkServ sv = server sv
vkTs sv = ts_s sv