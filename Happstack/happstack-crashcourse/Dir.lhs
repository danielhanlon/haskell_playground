<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="dir">Using <code>dir</code> to match on static path components</a></h3>

<p>We can use <code>dir</code> to handle components of the URI path which are static. For example, we might have a site with the two URLS <kbd>/hello</kbd> and <kbd>/goodbye</kbd>. </p>
<div class="code">

> module Main where
>
> import Control.Monad
> import Happstack.Server (nullConf, simpleHTTP, ok, dir)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello"    $ ok "Hello, World!"
>                                   , dir "goodbye"  $ ok "Goodbye, World!"
>                                   ]

</div>
<p class="source-code">[Source code for the app is <a href="Dir.hs">here.</a>]</p>

<p>If we start the app and point our browser at <a href="http://localhost:8000/hello">http://localhost:8000/hello</a> we get the hello message, and if we point it at <a href="http://localhost:8000/goodbye">http://localhost:8000/goodbye</a>, we get the goodbye message.</p>

