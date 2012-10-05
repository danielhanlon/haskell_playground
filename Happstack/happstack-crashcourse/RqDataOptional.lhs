<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="rqdataoptional">Looking up optional parameters</a></h2>

<p>Sometimes query parameters are optional. You may have noticed that the
<code>RqData</code> module does not seem to provide any functions for
dealing with optional values. That is because we can just use the <code>Alternative</code> class from <code>Control.Applicative</code> which provides the function <code>optional</code> for us:</p>

<div class="code">
#ifdef HsColour
> optional :: Alternative f => f a -> f (Maybe a)
#endif
</div>

<p>Here is a simple example where the <code>greeting</code> parameter is optional:

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>), optional)
> import Happstack.Server (ServerPart, look, nullConf, ok, simpleHTTP)
>
> helloPart :: ServerPart String
> helloPart =
>     do greet <- optional $ look "greeting"
>        ok $ (show greet)
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataError.hs">here.</a>]</p>
<p>If we visit <a href="http://localhost:8000/?greeting=hello">http://localhost:8000/?greeting=hello</a>, we will get <code>Just "hello"</code>.

<p>if we leave off the query parameters we get <a href="http://localhost:8000/">http://localhost:8000/</a>, we will get <code>Nothing</code>.
