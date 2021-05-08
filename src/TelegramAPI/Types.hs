module TelegramAPI.Types
    ( ResponseTelegram (..)
    , UpdateRes
    , Update (..)
    , Message (..)
    , CallbackQuery (..)
    , InlineKeyboardButton (..)
    , InlineKeyboardMarkup (..)
    ) where

import GHC.Generics  (Generic)
import Data.Aeson
import Data.Text     (Text) 

data ResponseTelegram a = ResponseTelegram 
    { result      :: a
    } deriving (Show, Eq, FromJSON, Generic)

type UpdateRes = ResponseTelegram [Update]

data Update = Update
    { update_id        :: Int
    , message          :: Maybe Message
    , callback_query   :: Maybe CallbackQuery
    } deriving (Show, Eq, FromJSON, Generic)

data Message = Message
    { message_id :: Int
    , from_id    :: Int
    , chat_id    :: Int
    , text       :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON Message where
    parseJSON = withObject "message" $ \v -> do
      message_id <- v .: "message_id"
      fromM      <- v .: "from"
      chat       <- v .: "chat"
      text       <- v .:? "text"
      from_id    <- fromM .: "id"
      chat_id    <- chat .: "id"
      return Message{..}

data CallbackQuery = CallbackQuery
    { from_id_query :: Int
    , data_query    :: Maybe String
    } deriving (Show, Eq)

instance FromJSON CallbackQuery where
    parseJSON = withObject "callback_query" $ \v -> do
      fromC         <- v .: "from"
      data_query    <- v .:? "data"
      from_id_query <- fromC .: "id"
      return CallbackQuery{..}

data InlineKeyboardMarkup = InlineKeyboardMarkup
    { inline_keyboard :: [[InlineKeyboardButton]]
    } deriving (Show, Generic, Eq, FromJSON, ToJSON)

data InlineKeyboardButton = InlineKeyboardButton
    { text_bttn          :: Text
    , callback_data_bttn :: Maybe Text
    } deriving (Show, Eq)

instance FromJSON InlineKeyboardButton where
    parseJSON = withObject "inline_keyboard" $ \v -> do
      text_bttn          <- v .: "text"
      callback_data_bttn <- v .: "callback_data"
      return InlineKeyboardButton {..}

instance ToJSON InlineKeyboardButton where
    toJSON InlineKeyboardButton{..} = 
      object [ "text"          .= text_bttn
             , "callback_data" .= callback_data_bttn ]
