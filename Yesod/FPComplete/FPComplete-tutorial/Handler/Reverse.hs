module Handler.Reverse where

import Import
import Data.List (reverse)

getReverseR :: text -> Handler Html
getReverseR theText =
    defaultLayout $ do
        $(widgetFile "reverse")
