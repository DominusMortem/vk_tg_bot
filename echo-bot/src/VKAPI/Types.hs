module VKAPI.Types 
    ( Init (..)
    , InitValue (..)
    , ErrorInit (..)
    , ResponseVK (..)
    , UpdateVK
    , Update (..)
    , ObjectVK (..)
    , Message (..)
    , Buttons (..)
    , Keyboard (..)
    , Action (..)
    ) where

import GHC.Generics  (Generic)
import Data.Aeson
import Data.Text     (Text)

data Init = Init
    { response :: Maybe InitValue
    , error :: Maybe ErrorInit
    } deriving (Show, Eq, Generic, FromJSON)

data InitValue = InitValue
    { key    :: String
    , server :: String
    , ts_s     :: String 
    } deriving (Show, Eq)

instance FromJSON InitValue where
    parseJSON = withObject "response" $ \v -> do
      key <- v .: "key"
      server <- v .: "server"
      ts_s <- v .: "ts"
      return InitValue{..}

data ErrorInit = ErrorInit
    { error_code :: Int
    , error_msg :: String
    } deriving (Show, Eq)

instance FromJSON ErrorInit where
    parseJSON = withObject "error" $ \v -> do
      error_code <- v .: "error_code"
      error_msg <- v .: "error_msg"
      return ErrorInit {..}

data ResponseVK a = ResponseVK
    { ts :: String
    , updates :: a
    } deriving (Show, Eq, Generic, FromJSON)

type UpdateVK = ResponseVK [Update]

data Update = Update
    { type_u :: String
    , object_u :: ObjectVK
    } deriving (Show, Eq)

instance FromJSON Update where
    parseJSON = withObject "updates" $ \v -> do
      type_u <- v .: "type"
      object_u <- v .: "object"
      return Update{..}

data ObjectVK = ObjectVK
    { message :: Maybe Message
    } deriving (Show, Eq, Generic, FromJSON)

data Message = Message
    { id_mes :: Int
    , from_id :: Int
    , random_id :: Int
    , text :: Text
    , payload :: Maybe String
    } deriving (Show, Eq)

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
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)
  
data Buttons = Buttons
  { action :: Action
  , color :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
  
data Action = Action
  { typeAct :: String
  , labelAct :: String
  , payloadAct :: String
  } deriving (Show, Eq, Generic)
  
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
