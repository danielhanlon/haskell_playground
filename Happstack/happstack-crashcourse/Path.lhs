<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="variable_path_segments">Matching on variable path segments</a></h2>

<h3><a name="path"><code>path</code></a></h3>

<p>Often times a path segment will contain a variable value we want to
extract and use, such as a number or a string. We can use the
<code>path</code> combinator to do that.</p>

<div class="code">
#ifdef HsColour
> path :: (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b
#endif
</div>

<p>You may find that type to be a little hard to follow because it is pretty abstract looking. Fortunately, we can look at it in an easier way. A <code>ServerPart</code> is a valid instance of, <code>ServerMonad m</code>, so we can just replace the <code>m</code> with <code>ServerPart</code>. You can do this anywhere you see type signatures with <code>(ServerMonad m) =></code> in them. In this case, the final result would look like:</p>

<div class="code">
#ifdef HsColour
> path :: (FromReqURI a) => (a -> ServerPart b) -> ServerPart b
#endif
</div>

<p><code>path</code> will attempt to extract and decode a path
segment, and if it succeeds, it will pass the decode value to the nested
server part.</p>

<p>Let's start with the most basic example, extracting a
<code>String</code> value. We will extend the Hello World server so
that we can say hello to anyone.</p>
<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum [ dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
>                                   ]

</div>
<p class="source-code">[Source code for the app is <a href="Path.hs">here.</a>]</p>

<p>Now, if we start the app and point our browser at:  <a href="http://localhost:8000/hello/World">http://localhost:8000/hello/World</a> we get the "Hello, World". </p>
<p>if we point it at <a href="http://localhost:8000/hello/Haskell">http://localhost:8000/hello/Haskell</a>, we get "Hello, Haskell".</p>
