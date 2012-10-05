<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="dirs">Using <code>dirs</code> as shorthand to match on multiple components</a></h3>

<p>As a shorthand, we can also use <code>dirs</code> to handle multiple static patch components.</p>
<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (nullConf, simpleHTTP, ok, dirs)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dirs "hello/world"  $ ok "Hello, World!"
>                                   , dirs "goodbye/moon" $ ok "Goodbye, Moon!"
>                                   ]

</div>
<p class="source-code">[Source code for the app is <a href="Dirs.hs">here.</a>]</p>

<p>If we start the app and point our browser at <a href="http://localhost:8000/hello/world">http://localhost:8000/hello/world</a> we get the hello message, and if we point it at <a href="http://localhost:8000/goodbye/moon">http://localhost:8000/goodbye/moon</a>, we get the goodbye message.</p>

