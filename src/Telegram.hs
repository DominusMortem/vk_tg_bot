module Telegram 
    ( startTG
    , newVarT
    ) where

import TelegramAPI.Types
import TelegramAPI.Methods
import Config
import Logger
import Control.Monad                  (when)
import Control.Monad.State            (execState)
import MapUser
import Network.HTTP.Simple            (Response, httpLBS, getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status      (statusCode, statusMessage)
import Data.Maybe                     (fromJust)
import Data.Aeson                     (decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M

data Variables = Variables
    { offset     :: Int
    , dictionary :: M.Map Int Int
    , update     :: [Update]
    } deriving (Show, Eq)

newVarT :: Variables
newVarT = Variables 
              { offset = 0
              , dictionary = M.empty
              , update = []
              }

response :: Config -> IO B.ByteString
response conf = do
    res <- httpLBS $ getUpdates conf
    errorResponse (loglevel conf) res
    let r = getResponseBody res
    when ((loglevel conf) <= Debug)
        $ debug' (loglevel conf) "Initialization Bot. Result response:" >> print r
    return r

responseOffset :: Config -> Variables -> IO B.ByteString
responseOffset conf vrb = do 
    let newOffset = offset vrb 
    res <- httpLBS $ getUpdatesWithOffset conf newOffset
    errorResponse (loglevel conf) res
    let r = getResponseBody res
    when ((loglevel conf) <= Debug) 
        $ debug' (loglevel conf) "Result responseOffset" >> print r
    return r

echoMes :: Config -> Variables -> IO () 
echoMes conf vrb = do
    let rep = repeats conf
    r <- mapM (copyM conf) $ echoM_ upd rep dict
    mapM_ (errorResponse (loglevel conf)) r
    let body = map getResponseBody r
    when (((loglevel conf) <= Info) && body /= []) 
        $ info' (loglevel conf) "Result response echoMes" 
        >> print (body)
    where copyM a x = httpLBS $ echoMessage a x
          upd = update vrb
          dict = dictionary vrb

helpMes :: Config -> Variables -> IO () 
helpMes conf vrb = do
    r <- mapM (hlp conf) $ helpM upd
    mapM_ (errorResponse (loglevel conf)) r
    let body = map getResponseBody r
    when (((loglevel conf) <= Info) && body /= []) 
        $ info' (loglevel conf) "Result response helpMes" 
        >> print (body)
    where hlp a x = httpLBS $ sendMessage a x
          upd = update vrb

repeatMes :: Config -> Variables -> IO () 
repeatMes conf vrb = do
    r <- mapM (rept conf $ dict) $ repeatM upd
    mapM_ (errorResponse (loglevel conf)) r
    let body = map getResponseBody r
    when (((loglevel conf) <= Info) && body /= []) 
        $ info' (loglevel conf) "Result response repeatMes" 
        >> print (body)
    where rept a d x = httpLBS $ keyboard a d x
          upd = update vrb
          dict = dictionary vrb

callbackMes :: Config -> Variables -> IO Variables 
callbackMes conf vrb = do
    let dict = dictionary vrb
        upd = update vrb
        query = callB upd
        dict' = execState (mapM_ mapSt $ idQuery query) dict
    when (((loglevel conf) <= Debug) && dict' /= dict) 
        $ debug' (loglevel conf) "Dictiory update:" 
        >> print (dict')
    return vrb {dictionary = dict'}
    where 
        idQuery query = map func query
        func x = (from_id_query x, read $ fromJust $ data_query x :: Int)

vib :: Config -> Variables -> IO B.ByteString
vib conf vrb = do
    let newOffset = offset vrb
    case newOffset of
        0 -> response conf
        _ -> responseOffset conf vrb
          
startTG :: Config -> Variables -> IO ()
startTG param vrb = do
    res <- vib param vrb
    let m = decode res :: Maybe UpdateRes
    case m of
        Nothing -> do
            when ((loglevel param) <= Debug) 
                $ error' (loglevel param) "Response error. Stop BOT."
            return ()
        Just resp -> do
            when ((loglevel param) <= Info)
                $ info' (loglevel param) "Result response 'UpdateRes':" >> print resp    
            let rez = result resp
            case rez of
                [] -> do
                    when ((loglevel param) <= Debug)
                        $ debug' (loglevel param) "Update empty. Waiting information."
                    startTG param vrb
                _ -> do
                    when ((loglevel param) <= Info)
                        $ info' (loglevel param) "Getting update. Result:" >> print rez
                    let up_id = last $ map update_id rez
                        vrb' = vrb {offset = up_id+1, update = rez}
                    when ((loglevel param) <= Debug) 
                        $ debug' (loglevel param) "Getting new offset: " >> print up_id
                        >> debug' (loglevel param) "Update Variables." >> print vrb'
                    echoMes param vrb'
                    helpMes param vrb'
                    repeatMes param vrb'
                    vrb'' <- callbackMes param vrb'
                    startTG param vrb''

errorResponse :: LogLvl -> Response a -> IO ()                       
errorResponse lg resp = do
    let body = getResponseStatus resp
    when ((lg <= Error) && (statusCode body /= 200 )) 
        $ error' lg "Response error" 
        >> print (statusCode body)
        >> print (statusMessage body)
