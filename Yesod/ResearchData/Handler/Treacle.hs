module Handler.Treacle where

import Import

getTreacleR :: Food -> Handler Html
getTreacleR f = defaultLayout [whamlet|<h1>Treacle likes #{show f}|]

postTreacleR :: Food -> Handler Html
postTreacleR = error "Not yet implemented: postTreacleR"
