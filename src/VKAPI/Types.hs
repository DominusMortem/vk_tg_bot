module VKAPI.Types where

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

data InitValue = InitValue
    { key    :: String
    , server :: String
    , ts_s     :: String 
    } deriving Show

instance FromJSON InitValue where
    parseJSON = withObject "response" $ \v -> do
      res <- v .: "response"
      key <- res .: "key"
      server <- res .: "server"
      ts_s <- res .: "ts"
      return InitValue{..}

data ResponseVK a = ResponseVK
    { ts :: String
    , updates :: a
    } deriving (Show, Generic, FromJSON)

type UpdateVK = ResponseVK [Update]

data Update = Update
    { type_u :: String
    , object_u :: ObjectVK
    } deriving Show

instance FromJSON Update where
    parseJSON = withObject "updates" $ \v -> do
      type_u <- v .: "type"
      object_u <- v .: "object"
      return Update{..}

data ObjectVK = ObjectVK
    { message :: Maybe Message
    } deriving (Show, Generic, FromJSON)

data Message = Message
    { id_mes :: Int
    , from_id :: Int
    , random_id :: Int
    , text :: Text
    , payload :: Maybe String
    } deriving Show

instance FromJSON Message where
    parseJSON = withObject "message" $ \v -> do
      id_mes <- v .: "id"
      from_id <- v .: "from_id"
      random_id <- v .: "random_id"
      text <- v .: "text"
      payload <- v .:? "payload"
      return Message{..}

data Keyboard = Keyboard
    { one_time :: Bool
    , buttons :: [[Buttons]]
    , inline :: Bool
    } deriving (Show, Generic, FromJSON, ToJSON)
  
data Buttons = Buttons
  { action :: Action
  , color :: String
  } deriving (Show, Generic, FromJSON, ToJSON)
  
data Action = Action
  { typeAct :: String
  , labelAct :: String
  , payloadAct :: String
  } deriving (Show, Generic)
  
instance FromJSON Action where
    parseJSON = withObject "action" $ \v -> do
      typeAct <- v .: "type"
      labelAct <- v .: "label"
      payloadAct <- v .: "payload"
      return Action {..}

instance ToJSON Action where
    toJSON Action{..} = 
      object [ "type"    .= typeAct
             , "label" .= labelAct
             , "payload" .= payloadAct ]