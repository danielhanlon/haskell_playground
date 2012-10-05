<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="helloRqData">Hello RqData</a></h2>

<p>Let's start with a simple <i>hello, world!</i> example that uses request parameters in the URL.</p>
<div class="code">

> module Main where
>
> import Happstack.Server (ServerPart, look, nullConf, simpleHTTP, ok)
>
> helloPart :: ServerPart String
> helloPart =
>     do greeting <- look "greeting"
>        noun     <- look "noun"
>        ok $ greeting ++ ", " ++ noun
>
> main :: IO ()
> main = simpleHTTP nullConf $ helloPart

</div>
<p class="source-code">[Source code for the app is <a href="HelloRqData.hs">here.</a>]</p>
<p>Now if we visit <a href="http://localhost:8000/?greeting=hello&amp;noun=rqdata">http://localhost:8000/?greeting=hello&amp;noun=rqdata</a>, we will get the message <i>hello, rqdata</i></p>

<p>we use the <code>look</code> function to look up some keys by name. The <code>look</code> function has the type:</p>

<div class="code">
#ifdef HsColour
> look :: (Functor m, Monad m, HasRqData m) => String -> m String
#endif
</div>

<p>Since we are using <code>look</code> in the <code>ServerPart</code> monad it has the simplified type:</p>

<div class="code">
#ifdef HsColour
> look :: String -> ServerPart String
#endif
</div>

<p>The <code>look</code> function looks up a key and decodes the associated value as a <code>String</code>. It assumes the underlying <code>ByteString</code> was utf-8 encoded. If you are using some other encoding, then you can use <code>lookBS</code> to construct your own lookup function.</p>

<p>If the key is not found, then <code>look</code> will fail. In <code>ServerPart</code> that means it will call <code>mzero</code>.

