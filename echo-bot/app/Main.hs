module Main where

import Telegram
import VK
import Config

main :: IO ()
main = do
    config <- getConfig
    case (api config) of
        "tg" -> startTG config newVarT
        "vk" -> startVK config newVar
        _    -> putStrLn "Close application."
