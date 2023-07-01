{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Environment (getEnv)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Time qualified

newtype Model = Model
  { publish :: Text -> IO ()
  }

data Action
  = Publish Text
  | Relay Text

relayBot :: Model -> BotApp Model Action
relayBot model =
  BotApp
    { botInitialModel = model,
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs = []
    }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ =
  case updateMessageText update of
    Just text -> Just (Publish text)
    Nothing -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Publish msg ->
    model <# do
      time <- liftIO Time.zonedString
      liftIO $ publish model (pack time <> " <Alan> " <> msg)
  Relay msg ->
    model <# do
      chatId <- liftIO $ getEnv "TELEGRAM_CHAT_ID"
      replyTo (SomeChatId (ChatId (read chatId))) (toReplyMessage msg)

-- Takes a function that triggers when the bot receives a message and returns a function that allows you to send messages from the bot to the user.
run :: (Text -> IO ()) -> IO (Text -> IO ())
run publish = do
  token <- getEnv "TELEGRAM_TOKEN"
  env <- defaultTelegramClientEnv (Token (pack token))
  actionF <- startBotAsync (relayBot (Model publish)) env
  return $ actionF . Relay
