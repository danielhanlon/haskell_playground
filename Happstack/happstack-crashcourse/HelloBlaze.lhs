<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="helloblaze">Using BlazeHtml</a></h2>

<p>It is trivial to use <a href="http://jaspervdj.be/blaze/">BlazeHtml</a> with Happstack. Essentially you just use <code>toResponse</code> to convert a blaze <code>Html</code> value into a <code>Response</code>. For more detailed information on using BlazeHtml, see the <a href="http://jaspervdj.be/blaze/">BlazeHtml website</a>. The following example should get you started:</p>

<div class="code">

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Happstack.Server
> import           Text.Blaze ((!))
> import qualified Text.Blaze.Html4.Strict as H
> import qualified Text.Blaze.Html4.Strict.Attributes as A
>
> appTemplate :: String -> [H.Html] -> H.Html -> H.Html
> appTemplate title headers body =
>     H.html $ do
>       H.head $ do
>         H.title (H.toHtml title)
>         H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
>         sequence_ headers
>       H.body $ do
>         body
>
> helloBlaze :: ServerPart Response
> helloBlaze = 
>    ok $ toResponse $ 
>     appTemplate "Hello, Blaze!" 
>                 [H.meta ! A.name "keywords" ! A.content "happstack, blaze, html"] 
>                 (H.p "hello, blaze!")
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloBlaze

</div>
<p class="source-code">[Source code for the app is <a href="HelloBlaze.hs">here.</a>]</p>

<p>Now if we visit <a href="http://localhost:8000/">http://localhost:8000/</a>, we will get an html page which says:

<pre class="code">
hello, blaze!
</pre>

<p>This example is pretty simple, but there are a few things to
note:</p>

<ul>
 <li>The <code>appTemplate</code> function is purely BlazeHtml code and is in no way Happstack specific.</li>
 <li>The existence of the <code>appTemplate</code> is purely a stylistic choice.</li>
 <li>I have found it useful to set the content-type meta tag.</li>
 <li>Happstack will automatically set the HTTP header <kbd>Content-Type: text/html; charset=UTF-8</kbd>. (BlazeHtml only supports UTF-8)</li>
</ul>