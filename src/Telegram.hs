module Telegram where

import TelegramAPI.Types
import TelegramAPI.Methods
import Control.Monad.State
import MapUser
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Data.Maybe (catMaybes, fromJust)
import Data.Aeson (decode)
import Data.ByteString.Lazy as B (ByteString)
import qualified Data.Map as M (Map, empty, findWithDefault)

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


vib :: Handle -> Config -> IO B.ByteString
vib hand conf = do
    let id = offset conf
    case id of
        0 -> initBot hand
        _ -> (updateBot hand) id
          
testIO :: Handle -> Config -> IO ()
testIO hand conf = do
    res <- vib hand conf
    let m = decode res :: Maybe UpdateRes
    case m of
        Nothing -> testIO hand conf
        Just response -> do
            let rez = result response
            case rez of
                [] ->  testIO hand conf
                _ -> do
                    let up_id = Prelude.last $ Prelude.map update_id rez
                    let conf' = conf {offset = up_id+1, update = rez}
                    echoMes conf'
                    helpMes conf'
                    repeatMes conf'
                    conf'' <- callbackMes conf'
                    print conf''
                    testIO hand conf''
                        
helpM cfg = Prelude.filter (\x -> text x == (Just "/help")) (catMaybes $ Prelude.map message (update cfg))
repeatM cfg = Prelude.filter (\x -> text x == (Just "/repeat")) (catMaybes $ Prelude.map message (update cfg))
callB cfg = (catMaybes $ Prelude.map callback_query (update cfg))

echoMes cfg = do
    mapM_ copyM $ echoM_ cfg
    return ()
    where copyM x = httpLBS $ echoMessage x

helpMes cfg = do
    mapM_ hlp $ helpM cfg
    return ()
    where hlp x = httpLBS $ sendMessage x

repeatMes cfg = do
    mapM_ repeats $ repeatM cfg
    return ()
    where repeats x = httpLBS $ keyboard x

callbackMes cfg = do
    let dict = dictionary cfg
        upd = callB cfg
        dict' = execState (mapM_ mapSt $ idQuery upd) dict
    return cfg {dictionary = dict'}

idQuery upd = Prelude.map func upd
    where func x = (from_id_query x, fromJust $ data_query x)

run :: IO ()
run = do
    testIO newHand newConf


echoM_ cfg = Prelude.concat (Prelude.map repeats (updates upd))
    where
        upd = update cfg
        rep = defaultRepeat cfg
        dict = dictionary cfg
        updates u = Prelude.filter (\x -> text x /= (Just "/help") && 
                                  text x /= (Just "/repeat")) 
                           (catMaybes $ Prelude.map message u)
        repeats x = Prelude.take (read $ newRepeat x :: Int) $ Prelude.repeat x
        newRepeat x = M.findWithDefault (rep)
                                      (from_id x) dict
