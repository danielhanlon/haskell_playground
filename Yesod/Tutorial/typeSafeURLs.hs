{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Renderer.String (renderHtml)
import Data.Text (Text)

data MyRoute = Home

render :: MyRoute -> [(Text,Text)] -> Text
render Home _ = "/home"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
  Return to #
  <a href=@{Home}>Homepage
  .
|]

main :: IO ()
main = putStrLn $ renderHtml $ [hamlet|
<body>
  <p>This is my page.
  ^{footer}
|] render
