module Handler.Echo2 where

import Import

getEcho2R :: Text -> Handler Html
getEcho2R theText = defaultLayout $(widgetFile "echo")
