<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="dir2">Using <code>dir</code> to match on multiple components</a></h3>

<p>We can match on multiple components by chaining calls to <code>dir</code> together</p> 
<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello"    $ dir "world" $ ok "Hello, World!"
>                                   , dir "goodbye"  $ dir "moon"  $ ok "Goodbye, Moon!"
>                                   ]

</div>
<p class="source-code">[Source code for the app is <a href="Dir2.hs">here.</a>]</p>

<p>If we start the app and point our browser at <a href="http://localhost:8000/hello/world">http://localhost:8000/hello/world</a> we get the hello message, and if we point it at <a href="http://localhost:8000/goodbye/moon">http://localhost:8000/goodbye/moon</a>, we get the goodbye message.</p>

