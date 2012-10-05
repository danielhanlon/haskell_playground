<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="rqdataread">Using Read</a></h3>

<p>For some types, it is sufficient to use <code>Read</code> to parse
the <code>String</code> into a value. <code>RqData</code> provides
functions such as <code>lookRead</code> to assist with this. The advantage of using <code>lookRead</code> instead of calling <code>look</code> and applying <code>read</code> yourself is that <code>lookRead</code> ties into the <code>RqData</code> error handling system neatly.</p>

<div class="code">
#ifdef HsColour
> lookRead :: (Functor m, Monad m, HasRqData m, Read a) => String -> m a
#endif
</div>

<p>Here is a trivial example where we create a <code>lookInt</code> function which looks for an <code>Int</code> parameter named <kbd>int</kbd>.

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, lookRead, getDataFn)
>
> lookInt :: RqData Int
> lookInt = lookRead "int"
>
> intPart :: ServerPart String
> intPart =
>     do r <- getDataFn lookInt
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "Read the int: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ intPart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataRead.hs">here.</a>]</p>

<p>Now if we visit <a href="http://localhost:8000/?int=1">http://localhost:8000/?int=1</a>, we will get the message:

<pre class="code">
Read the int: 1
</pre>

<p>If we visit <a href="http://localhost:8000/?int=apple">http://localhost:8000/?int=apple</a>, we will get the error:

<pre class="code">
Read failed while parsing: apple
</pre>
