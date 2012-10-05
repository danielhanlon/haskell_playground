<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="method">Matching on Request Method <code>(GET, POST, etc)</code></a></h2>
<h3><a name="methodM"><code>methodM</code> and <code>methodOnly</code></a></h3>
<p>We can specify that a route is only valid for specific HTTP request methods by using the <code>methodM</code> guard:</p>

<div class="code">
#ifdef HsColour
> methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
#endif
</div>

<p>The <code>methodM</code> guard does two things:</p>
<ol>
 <li>Specifies what methods to allow</li>
 <li>Checks that all the path segments have been consumed</li>
</ol>

<p>Here is a simple demo app:</p>
<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Happstack.Server (Method(GET, POST), dir, methodM, nullConf, ok, simpleHTTP)
>
> main :: IO ()
> main = simpleHTTP nullConf $ msum 
>        [ do methodM GET
>             ok $ "You did a GET request.\n"
>        , do methodM POST
>             ok $ "You did a POST request.\n"
>        , dir "foo" $ do methodM GET
>                         ok $ "You did a GET request on /foo\n"
>        ]
>

</div>
<p class="source-code">[Source code for the app is <a href="MethodM.hs">here.</a>]</p>

<p>Using <code>curl</code> we can see the expected results for normal <code>GET</code> and <code>POST</code> requests to <code>/</code>:</p>

<div class="code">
<pre>
 $ curl http://localhost:8000/
You did a GET request.
 $ curl -d '' http://localhost:8000/
You did a POST request.
</pre>
</div>

<p>Note that <code>methodM</code> also requires that the all the segments of request path have been consumed. We can see in here that <code>/foo</code> is accepted, but not <code>/foo/bar</code>, since only <code>foo</code> is consumed by the, <code>dir "foo"</code> filter.</p>

<div class="code">
<pre>
 $ curl http://localhost:8000/foo
You did a GET request on /foo
 $ curl http://localhost:8000/foo/bar
&lt;404 message&gt;
</pre>
</div>

<p>If we want to allow unconsumed path segments we can use <code>methodOnly</code> instead. </p>
<div class="code">
#ifdef HsColour
> methodOnly :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
#endif
</div>

<p>In fact the definition for <code>methodM</code> is simply:</p>
<div class="code">
#ifdef HsColour
> methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
> methodM meth = methodOnly meth >> nullDir
#endif
</div>
