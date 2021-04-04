module Telegram where

import BOTAPI
import TelegramAPI.Types
import TelegramAPI.Methods
import MapUser
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

new :: Handle
new = Handle { initBot = response
             , updateBot = responseOffset
             , offset = 0
             , dictonary = M.empty
             }

response :: IO B.ByteString
response = do
    res <- httpLBS getUpdates
    return (getResponseBody res)

responseOffset :: Int -> IO B.ByteString
responseOffset id = do 
    res <- httpLBS $ getUpdatesWithOffset id
    return (getResponseBody res)

vib :: Handle -> IO B.ByteString
vib hand = do
    let id = offset hand
    case id of
        0 -> initBot hand
        _ -> (updateBot hand) id
          
testIO :: Handle -> IO ()
testIO hand = do
    res <- vib hand
    let m = decode res :: Maybe UpdateRes
    case m of
        Nothing -> testIO hand
        Just response -> do
            let rez = result response
            case rez of
                [] -> testIO hand
                (x:xs) -> do
                  let up_id = update_id x
                  let dict = dictonary hand
                  if (message x) == Nothing
                  then do
                    let cb = callback_query x
                    let d = fromJust $ data_query $ fromJust cb
                    let a = from_id_query $ fromJust cb
                    let dict' = (testMap' a d dict)
                    let hand' = hand {dictonary = dict', offset = (up_id + 1)}
                    print dict'
                    testIO hand'
                  else do
                    let mes = message x
                    let t = textT mes
                    let u = fromId mes
                    let dict' = (testMap u "1" dict)
                    let hand' = hand {dictonary = dict', offset = (up_id + 1)}
                    print dict'
                    case t of
                      "/help" -> do
                        httpLBS $ sendMessage mes
                        testIO hand'
                      "/repeat" -> do
                        httpLBS $ keyboard mes
                        testIO hand'
                      _ -> do
                        let rep_ = read $ fromJust $ (M.lookup u dict') :: Int
                        let rep = Prelude.take rep_ (Prelude.repeat (echoMessage mes)) 
                        mapM_ httpLBS rep
                        print (up_id + 1)
                        testIO hand'

