module Telegram where

import TelegramAPI.Types
import TelegramAPI.Methods
import Control.Monad.State            (State, execState)
import MapUser
import Network.HTTP.Simple            (httpLBS, getResponseBody)
import Data.Maybe                     (catMaybes, fromJust)
import Data.Aeson                     (decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M

data Config = Config
    { offset :: Int
    , dictionary :: M.Map Int String
    , update :: [Update]
    , defaultRepeat :: String
    } deriving (Show, Eq)

data Handle = Handle 
    { initBot  :: IO B.ByteString
    , updateBot :: Int -> IO B.ByteString
    }

newConf :: Config
newConf = Config { offset = 0
                 , dictionary = M.empty
                 , update = []
                 , defaultRepeat = "1"
                 }

newHand :: Handle
newHand = Handle { initBot = response
                 , updateBot = responseOffset
                 }
         
response :: IO B.ByteString
response = do
    res <- httpLBS getUpdates
    return (getResponseBody res)

responseOffset :: Int -> IO B.ByteString
responseOffset id = do 
    res <- httpLBS $ getUpdatesWithOffset id
    return (getResponseBody res)

echoMes :: Config -> IO () 
echoMes cfg = do
    mapM_ copyM $ echoM_ upd rep dict
    return ()
    where copyM x = httpLBS $ echoMessage x
          upd = update cfg
          rep = defaultRepeat cfg
          dict = dictionary cfg

helpMes :: Config -> IO () 
helpMes cfg = do
    mapM_ hlp $ helpM upd
    return ()
    where hlp x = httpLBS $ sendMessage x
          upd = update cfg

repeatMes :: Config -> IO () 
repeatMes cfg = do
    mapM_ repeats $ repeatM upd
    return ()
    where repeats x = httpLBS $ keyboard x
          upd = update cfg

callbackMes :: Config -> IO Config 
callbackMes cfg = do
    let dict = dictionary cfg
        upd = update cfg
        query = callB upd
        dict' = execState (mapM_ mapSt $ idQuery query) dict
    return cfg {dictionary = dict'}
    where 
        idQuery query = map func query
        func x = (from_id_query x, fromJust $ data_query x)

vib :: Handle -> Config -> IO B.ByteString
vib hand conf = do
    let id = offset conf
    case id of
        0 -> initBot hand
        _ -> (updateBot hand) id
          
startTG :: Handle -> Config -> IO ()
startTG hand conf = do
    res <- vib hand conf
    let m = decode res :: Maybe UpdateRes
    case m of
        Nothing -> startTG hand conf
        Just response -> do
            let rez = result response
            case rez of
                [] ->  startTG hand conf
                _ -> do
                    let up_id = last $ map update_id rez
                    let conf' = conf {offset = up_id+1, update = rez}
                    echoMes conf'
                    helpMes conf'
                    repeatMes conf'
                    conf'' <- callbackMes conf'
                    print conf''
                    startTG hand conf''

run :: IO ()
run = do
    startTG newHand newConf
