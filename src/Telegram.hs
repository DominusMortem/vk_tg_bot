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

new :: IO Handle
new = return $ Handle { initBot = response
                      , updateBot = responseOffset
                      , offset = 0}

response :: IO B.ByteString
response = do
    res <- httpLBS getUpdates
    return (getResponseBody res)

responseOffset :: Int -> IO B.ByteString
responseOffset id = do 
    res <- httpLBS $ getUpdatesWithOffset id
    return (getResponseBody res)
{--
vib :: Int -> IO B.ByteString
vib id = do
    case id of
        0 -> response
        _ -> responseOffset id--}
          
testIO :: Handle -> M.Map Int String -> IO ()
testIO hand dict = do
    res <- vib id
    let m = decode res :: Maybe UpdateRes
    case m of
        Nothing -> testIO id dict
        Just response -> do
            let rez = result response
            case rez of
                [] -> testIO id dict
                (x:xs) -> do
                  let up_id = update_id x
                  if (message x) == Nothing
                  then do
                    let cb = callback_query x
                    let d = fromJust $ data_query $ fromJust cb
                    let a = from_id_query $ fromJust cb
                    let dict' = (testMap' a d dict)
                    print dict'
                    testIO (up_id + 1) dict'
                  else do
                    let mes = message x
                    let t = textT mes
                    let u = fromId mes
                    let dict' = (testMap u "1" dict)
                    print dict'
                    case t of
                      "/help" -> do
                        httpLBS $ sendMessage mes
                        testIO (up_id + 1) dict'
                      "/repeat" -> do
                        httpLBS $ keyboard mes
                        testIO (up_id + 1) dict'
                      _ -> do
                        let rep_ = read $ fromJust $ (M.lookup u dict') :: Int
                        let rep = Prelude.take rep_ (Prelude.repeat (echoMessage mes)) 
                        forM_ rep httpLBS -- $ echoMessage mes
                        print (up_id + 1)
                        testIO (up_id + 1) dict'

