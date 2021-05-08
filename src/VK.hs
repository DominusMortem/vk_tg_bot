module VK 
    ( startVK
    , newVar 
    ) where



import VKAPI.Methods
import VKAPI.Types
import MapUser
import Config
import Logger
import Control.Monad                  (when)
import Control.Monad.State            (execState)
import Network.HTTP.Simple            (Response, httpLBS, getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status      (statusCode, statusMessage)
import Data.Maybe                     (fromJust)
import Data.Aeson                     (decode)
import Prelude hiding                 (error)
import qualified Data.Map             as M

data Variables = Variables
    { session    :: InitValue
    , dictionary :: M.Map Int Int
    , update     :: [Update]
    , vkerror    :: String
    } deriving (Show, Eq)

newVar :: Variables
newVar = Variables
    { session    = InitValue "" "" ""
    , dictionary = M.empty
    , update     = []
    , vkerror    = ""
    }

payloadMes :: Config -> Variables -> IO Variables 
payloadMes conf vrb = do
    let dict = dictionary vrb
        upd = update vrb
        query = payloadM upd
        dict' = execState (mapM_ mapSt $ idQuery query) dict
    when (((loglevel conf) <= Debug) && dict' /= dict) 
        $ debug' (loglevel conf) "Dictiory update:" 
        >> print (dict')
    return vrb {dictionary = dict'}
    where 
        idQuery query = map func query
        func x = (from_id x, read $ fromJust $ payload x :: Int)

echoMes :: Config -> Variables -> IO () 
echoMes conf vrb = do
    let rep = repeats conf 
    r <- mapM (copyM  conf) $ echoM upd rep dict
    mapM_ (errorResponse (loglevel conf)) r
    let body = map getResponseBody r
    when (((loglevel conf) <= Info) && body /= []) 
        $ info' (loglevel conf) "Result response echoMes" 
        >> print (body)
    where copyM a x = httpLBS $ echoMessage a x
          upd = update vrb
          dict = dictionary vrb

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

initVK :: Config -> Variables -> IO Variables
initVK conf vrb = do
    when ((loglevel conf) <= Debug) 
        $ debug' (loglevel conf) "Initialization VK....."
    initRes <- httpLBS $ getLongPollServer conf
    errorResponse (loglevel conf) initRes
    let bodyRes = getResponseBody initRes
        decRes = decode bodyRes :: Maybe Init
    when ((loglevel conf) <= Debug) 
        $ debug' (loglevel conf) "Result initVK" >> print bodyRes
    case decRes of
        Nothing -> initVK conf vrb
        Just initial -> do
            if (response initial) /= Nothing
            then do
                let vrb' = vrb {session = fromJust $ response initial}
                return vrb'
            else do
                let vkerr = error_msg $ fromJust $ error initial
                return vrb {vkerror = vkerr}

startVK :: Config -> Variables -> IO ()
startVK conf vrb = do
    case (ts_s $ session vrb) of
        "" -> do
            initconf <- initVK conf vrb
            if (vkerror initconf /= "")
            then print $ vkerror initconf
            else startVK conf initconf
        _ -> do
            let sess = session vrb
            updRes <- httpLBS $ getLongPollAPI sess conf
            errorResponse (loglevel conf) updRes
            let bodyRes = (getResponseBody updRes)
                decRes = decode bodyRes :: Maybe UpdateVK
            when ((loglevel conf) <= Debug) 
                $ debug' (loglevel conf) "Result getLongPoll" >> print bodyRes
            case decRes of 
                Nothing -> do
                    when ((loglevel conf) <= Info) 
                        $ error' (loglevel conf) "Response error. Stop BOT."
                        >> return ()
                Just start -> do
                    let tsNew = ts start
                        upd = updates start
                        vrb' = vrb {update = upd, session = sess {ts_s = tsNew}}
                    when (((loglevel conf) <= Debug) && (vrb' /= vrb))
                        $ debug' (loglevel conf) "Update Variables" >> print vrb'
                    helpMes conf vrb'
                    repeatMes conf vrb'
                    echoMes conf vrb'
                    vrb'' <- payloadMes conf vrb'
                    startVK conf vrb''

errorResponse :: LogLvl -> Response a -> IO ()                       
errorResponse lg resp = do
    let body = getResponseStatus resp
    when ((lg <= Error) && (statusCode body /= 200 )) 
        $ error' lg "Response error" 
        >> print (statusCode body)
        >> print (statusMessage body)
