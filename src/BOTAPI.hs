module BOTAPI where

import Data.ByteString.Lazy as B

data Handle = Handle 
    { initBot  :: IO B.ByteString
    , updateBot :: Int -> IO B.ByteString
    , offset :: Int
    , dictonary :: M.Map Int String
    }
