module Config 
    ( Config (..)
    , getConfig
    ) where


import Logger
import Control.Monad                  (when)
import Data.Configurator as C
import qualified Data.Text as T

data Config = Config
    { api :: !String
    , group_id :: !Int
    , loglevel :: !LogLvl
    , token :: !String
    , repeats :: !Int
    , messagehelp :: !String
    , messagerepeat :: !String
    , timeout :: !Int
    } deriving (Show, Eq)

getConfig :: IO Config
getConfig = do
    conf <- C.load [C.Optional "bot.conf"]
    logstr <- C.lookupDefault "Off" conf (T.pack "loglevel") :: IO String
    let loglevel = case logstr of
                        "Debug"   -> Debug
                        "Info"    -> Info
                        "Warning" -> Warning
                        "Error"   -> Error
                        _         -> Off
    api <- C.lookupDefault "vk" conf $ T.pack "api" :: IO String
    group_id <- C.lookupDefault 0 conf (T.pack "group_id") :: IO Int
    token <- getToken conf api loglevel
    repeats <- C.lookupDefault 1 conf (T.pack "repeat") :: IO Int
    messagehelp <- C.lookupDefault "" conf (T.pack "messagehelp") :: IO String
    messagerepeat <- C.lookupDefault "" conf (T.pack "messagerepeat") :: IO String
    timeout <- C.lookupDefault 0 conf (T.pack "timeout") :: IO Int
    return (Config api group_id loglevel token repeats messagehelp messagerepeat timeout) 

    where
        getToken cfg api logL = do
            case api of
                "vk" -> do
                    when (logL <= Info) 
                        $ info' logL "Load VK token."
                    tokenVK <- C.lookupDefault "" cfg (T.pack "tokenVK") :: IO String
                    when ((logL <= Warning) && ((length tokenVK) /= 85))
                        $ warning' logL "Token may be wrong"
                    return tokenVK
                "tg" -> do
                    when (logL <= Info)
                        $ info' logL "Load Telegram token."
                    tokenTG <- C.lookupDefault "" cfg (T.pack "tokenTG") :: IO String
                    when ((logL <= Warning) && ((length tokenTG) /= 46))
                        $ warning' logL "Token may be wrong"
                    return tokenTG
                _    -> do
                    when (logL <= Error)
                        $ error' logL "No API"
                    return ""
