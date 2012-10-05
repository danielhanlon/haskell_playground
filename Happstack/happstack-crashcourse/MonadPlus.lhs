<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="monadplus">Choosing between multiple <code>ServerPartTs</code></a></h2>

<p>In the first example, we had only one <code>ServerPartT</code>. All
<code>Request</code>s were handled by the same part and returned the
same <code>Response</code>.

<p>In general, our applications will have many
<code>ServerPartT</code>s. We combine them into a single top-level
<code>ServerPartT</code> by using <code>MonadPlus</code>. Typically
via the <code>msum</code> function:

<div class="code">
#ifdef HsColour
> msum :: (MonadPlus m) => [m a] -> m a
#endif
</div>

<p>In the following example we combine three <code>ServerPartT</code>s
together.</p>

<div class="code">

> module Main where
>
> import Control.Monad
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ mzero
>                                   , ok "Hello, World!"
>                                   , ok "Unreachable ServerPartT"
>                                   ]

</div>
<p class="source-code">[Source code for the app is <a href="MonadPlus.hs">here.</a>]</p>

<p>The behaviour of <code>MonadPlus</code> is to try each <code>ServerPartT</code> in succession, until one succeeds.</p>

<p>In the example above, the first part is <code>mzero</code>, so it will always fail. The second part will always succeed. That means the third part will never be reachable.</p>

<p>Alas, that means this application will appear to behave exactly like the first application. What we need are some ways to have parts match or fail depending on the contents of the http <code>Request</code>.
