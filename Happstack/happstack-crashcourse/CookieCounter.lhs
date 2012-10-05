<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="simple_cookie_demo">Simple Cookie Demo</a></h3>

<p>The cookie interface is pretty small. There are two parts to the interface: setting a cookie and looking up a cookie.</p>

<p>To create a <code>Cookie</code> value, we use the <code>mkCookie</code> function:</p>

<div class="code">
#ifdef HsColour
> -- | create a 'Cookie'
> mkCookie  :: String -- ^ cookie name
>           -> String -- ^ cookie value
>           -> Cookie
>
#endif
</div>

<p>Then we use the <code>addCookie</code> function to send the cookie to the user. This adds the <code>Set-Cookie</code> header to the <code>Response</code>. So the cookie will not actually be set until the <code>Response</code> is sent.</p>

<div class="code">
#ifdef HsColour
> -- | add the 'Cookie' to the current 'Response'
> addCookie :: (MonadIO m, FilterMonad Response m) => CookieLife -> Cookie -> m ()
>
#endif
</div>

<p>The first argument of <code>addCookie</code> specifies how long the browser should keep the cookie around. See the <a href="#cookie_life">cookie lifetime</a> section for more information on <code>CookieLife</code>.</p>

<p>To lookup a cookie, we use some <code>HasRqData</code> functions. There are only three cookie related functions:</p>

<div class="code">
#ifdef HsColour
> -- | lookup a 'Cookie'
> lookCookie :: (Monad m, HasRqData m) => 
>               String -- ^ cookie name
>            -> m Cookie
>
> -- | lookup a 'Cookie' and return its value
> lookCookieValue :: (Functor m, Monad m, HasRqData m) => 
>                    String -- ^ cookie name
>                 -> m String
>
> -- | look up a 'Cookie' value and try to convert it using 'read'
> readCookieValue :: (Functor m, Monad m, HasRqData m, Read a) => 
>                    String -- ^ cookie name
>                 -> m a
#endif
</div>

<p>The cookie functions work just like the other <code>HasRqData</code> functions. That means you can use <code>checkRq</code>, etc.</p>

<p>The following example puts all the pieces together. It uses the cookie to store a simple counter specifying how many requests have been made:</p>

<div class="code">

> module Main where
> import Control.Monad
> import Control.Monad.Trans
> import Happstack.Server
> import Control.Monad     ( msum )
> import Happstack.Server  ( CookieLife(Session), ServerPart, addCookie
>                          , look, mkCookie, nullConf, ok, readCookieValue
>                          , simpleHTTP )
>
> homePage :: ServerPart String
> homePage =
>     msum [ do rq <- askRq
>               liftIO $ print (rqPaths rq)
>               mzero
>          , do requests <- readCookieValue "requests"
>               addCookie Session (mkCookie "requests" (show (requests + 1)))
>               ok $ "You have made " ++ show requests ++ " requests to this site."
>          , do addCookie Session (mkCookie "requests" (show 2))
>               ok $ "This is your first request to this site."
>          ]
>
> main :: IO ()
> main = simpleHTTP nullConf $ homePage

</div>
<p class="source-code">[Source code for the app is <a href="CookieCounter.hs">here.</a>]</p>

<p>Now if you visit <a href="http://localhost:8000/">http://localhost:8000/</a> you will get a message like:</p>

<div class="code">
<pre>
This is your first request to this site.
</pre>
</div>

<p>If you hit reload you will get:</p>

<div class="code">
<pre>
You have made 3 requests to this site.
</pre>
</div>

<p>Now wait a second! How did we go from 1 to 3, what happened to 2? The browser will send the cookie with every request it makes to the server. In this example, we ignore the request path and send a standard response to every request that is made. The browser first requests the page, but it also requests the <code>favicon.ico</code> for the site. So, we are really getting two requests everytime we load the page. Hence the counting by twos. It is important to note that the browser does not just send the cookie when it is expecting an html page -- it will send it when it is expecting a jpeg, a css file, a js, or anything else.</p>

<p>There is also a race-condition bug in this example. See the <a href="#cookie_race">cookie issues section</a> for more information.</p>

