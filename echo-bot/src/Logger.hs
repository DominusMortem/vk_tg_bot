module Logger 
    ( LogLvl (..)
    , debug'
    , info'
    , warning'
    , error'
    ) where

data LogLvl = Debug 
            | Info
            | Warning
            | Error
            | Off
            deriving (Eq, Ord, Show)

log' :: LogLvl -> LogLvl -> String -> String -> IO ()
log' cfglog loglvl lg str
    | loglvl >= cfglog = putStrLn (concat [take 10 $ repeat ('-')
                                          , lg
                                          , " "
                                          , str
                                          , " "
                                          , lg
                                          , take 10 $ repeat ('-')])
    | otherwise = return () 

debug', info', warning', error' :: LogLvl -> String -> IO ()
debug' cfglog = log' cfglog Debug "[Debug]"
info' cfglog = log' cfglog Info "[Info]"
warning' cfglog = log' cfglog Warning "[Warning]"
error' cfglog = log' cfglog Error "[Error]"