<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="rqdataerror">Using the <code>RqData</code> Monad for better error reporting</a></h2>

<p>So far we have been using the <code>look</code> function in the
<code>ServerPart</code> monad. This means that if any
<code>look</code> fails, that handler fails. Unfortunately, we are not
told what parameter was missing -- which can be very frustrating when
you are debugging your code. It can be even more annoying if you are
providing a web service, and whenever a developer forgets a parameter,
they get a 404 with no information about what went wrong.</p>

<p>So, if we want better error reporting, we can use functions like
<code>look</code> in the <code>RqData Applicative Functor</code>.

<p>We can use <code>getDataFn</code> to run the <code>RqData</code>:

<div class="code">
#ifdef HsColour
> getDataFn :: (HasRqData m, ServerMonad m, MonadIO m) =>
>              RqData a
>           -> m (Either [String] a)
#endif
</div>

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, look, getDataFn)
>
> helloRq :: RqData (String, String)
> helloRq =
>     (,) <$> look "greeting" <*> look "noun"
>
> helloPart :: ServerPart String
> helloPart =
>     do r <- getDataFn helloRq
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right (greet, noun)) ->
>              ok $ greet ++ ", " ++ noun
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataError.hs">here.</a>]</p>
<p>If we visit <a href="http://localhost:8000/?greeting=hello&amp;noun=world">http://localhost:8000/?greeting=hello&amp;noun=world</a>, we will get our familiar greeting <kbd>hello, world</kbd>.
<p>But if we leave off the query parameters <a href="http://localhost:8000/">http://localhost:8000/</a>, we will get a list of errors:

<pre class="code">
Parameter not found: greeting
Parameter not found: noun
</pre>

<p>We could use the <code>Monad</code> instance <code>RqData</code> to build the request. However, the monadic version will only show us the <b>first</b> error that is encountered. So would have only seen that the <code>greeting</code> was missing. Then when we added a <code>greeting</code> we would have gotten a new error message saying that <code>noun</code> was missing.</p>

<p>In general, improved error messages are not going to help people visiting your website. If the parameters are missing it is because a form or link they followed is invalid. There are two places where there error messages are useful:</p>
<ol>
 <li>When you are developing and debugging your site</li>
 <li>Reporting errors to users of your web service API</li>
</ol>

<p>If you are providing a REST API for developers to use, they are going to be a lot happier if they get a detailed error messages instead of a plain old 404.</p>
