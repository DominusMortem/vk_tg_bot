module VK where

import MapUser
import VKAPI.Methods
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
import qualified Data.Map as M

responseVK :: IO B.ByteString
responseVK = do
    res <- httpLBS getLongPollServer
    return (getResponseBody res)

responseStart :: String -> String -> Int -> IO B.ByteString
responseStart sv key_ tS = do
    res <- httpLBS $ getLongPollAPI sv key_ tS
    return (getResponseBody res)

initVK :: IO ()
initVK = do
    res <- responseVK
    let m = decode res :: Maybe InitValue
    case m of
        Nothing -> initVK
        Just response -> do
            let tS = read (vkTs response) :: Int
            let serv = server response
            let key_ = key response
            startVK serv key_ (tS) testData

startVK :: String -> String -> Int -> M.Map Int String -> IO ()
startVK serv key_ tS dict = do
    start <- responseStart serv key_ tS
    let m = decode start :: Maybe UpdateVK
    case m of 
        Nothing -> startVK serv key_ tS dict
        Just response -> do
            let tSnew = read (ts response) :: Int
            let upd = updates response
            case upd of
                [] -> startVK serv key_ tSnew dict
                (x:xs) -> do
                    let typeMes = type_u x
                    case typeMes of
                        "message_new" -> do
                            let mesVK = fromJust $ message $ object_u x
                            let command = text mesVK
                            let numRep = payload mesVK
                            let (idMes, usid, randId) = sendData mesVK
                            let dict' = (testMap usid "1" dict)
                            case numRep of
                                Nothing -> do
                                  case command of
                                    "/help" -> do
                                        httpLBS $ sendMessage usid randId
                                        startVK serv key_ tSnew dict'
                                    "/repeat" -> do
                                        httpLBS $ keyboard usid randId
                                        startVK serv key_ tSnew dict'
                                    _ -> do
                                        let rep_ = read $ fromJust $ (M.lookup usid dict') :: Int
                                        let rep = Prelude.take rep_ (Prelude.repeat (echoMessage idMes usid randId)) 
                                        forM_ rep httpLBS
                                        startVK serv key_ tSnew dict'
                                Just pay -> do
                                    let dict' = (testMap' usid pay dict)
                                    print dict'
                                    startVK serv key_ tSnew dict'
                        _ -> startVK serv key_ tSnew dict

sendData :: Message -> (Int, Int, Int)
sendData mess = (idMes, usid, randId)
    where idMes = id_mes mess
          usid = from_id mess
          randId = random_id mess