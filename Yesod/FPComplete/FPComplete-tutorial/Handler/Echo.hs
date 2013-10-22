module Handler.Echo where

import Import

getEchoR :: String -> Handler RepHtml
getEchoR theText = 
    defaultLayout $ do
        [whamlet|<h1>#{theText}|]
