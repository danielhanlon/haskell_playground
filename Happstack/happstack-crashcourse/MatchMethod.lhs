<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="matchMethod">Advanced method matching with <code>MatchMethod</code></a></h3>

<p>The method routing functions use a class <code>(MatchMethod method)</code> instead of the concrete type <code>Method</code>.</p>

<div class="code">
#ifdef HsColour
> class MatchMethod m where 
>     matchMethod :: m -> Method -> Bool
>
> instance MatchMethod Method           where ...
> instance MatchMethod [Method]         where ...
> instance MatchMethod (Method -> Bool) where ...
> instance MatchMethod ()               where ...
#endif
</div>

<p>This allows us to easily match on more than one method by either providing a list of acceptable matches, or by providing a function which returns a boolean value. We can use this feature to support the <code>HEAD</code> method. When the client does a <code>HEAD</code> request, the server is supposed to return the same headers it would for a GET request, but with an empty response body. Happstack includes special support for handling this automatically in most cases.</p>

<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (Method(GET, HEAD), dir, methodM, nullConf, ok, simpleHTTP)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum 
>        [ do methodM [GET, HEAD]
>             ok $ "Hello, World\n"
>        ]
>

</div>
<p class="source-code">[Source code for the app is <a href="MatchMethod.hs">here.</a>]</p>

<p>We can now use curl to do a normal <code>GET</code> request, or we can
use the <code>-I</code> flag which does a <code>HEAD</code> request:</p>

<div class="code">
<pre>
 $ curl http://localhost:8000/
Hello, World
 $ curl -I http://localhost:8000/
HTTP/1.1 200 OK
Connection: Keep-Alive
Content-Length: 13
Content-Type: text/plain; charset=UTF-8
Date: Tue, 15 Jun 2010 19:56:07 GMT
Server: Happstack/0.5.0
</pre>
</div>

<p>Happstack automatically notices that it is a <code>HEAD</code> request, and does not send the body.</p>
