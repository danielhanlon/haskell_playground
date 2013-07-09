module Handler.Tara where

import Import

getTaraR :: Food -> Handler Html
getTaraR f = defaultLayout [whamlet|<h1>Tara likes #{show f}|]
