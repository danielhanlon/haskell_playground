module Handler.Blog ( getBlogR
                    , postBlogR
                    , getArticleR
                    )
where

import Import
import Data.Monoid
import Yesod.Form.Nic (YesodNic, nicHtmlField)

instance YesodNic App

getBlogR :: Handler Html
getBlogR = error "Not yet implemented: getBlogR"

postBlogR :: Handler Html
postBlogR = error "Not yet implemented: postBlogR"
