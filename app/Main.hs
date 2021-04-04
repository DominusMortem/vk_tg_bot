module Main where

import Network.HTTP.Simple
import Telegram
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
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map as M


main = print "Hello World"