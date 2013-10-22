module Handler.Echo where

import Import

getEchoR :: String -> Handler RepHtml
getEchoR theText = 
    defaultLayout $ [whamlet|<h1>The text is:#{theText}|]
