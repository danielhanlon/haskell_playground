<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" 
          "http://www.w3.org/TR/html4/strict.dtd">
<html>
 <head>
  <title>Crash Course in Happstack</title>
  <link type='text/css' rel='stylesheet' href='hscolour.css'>
  <link type='text/css' rel='stylesheet' href='blog2.css'>
  <link type='text/css' rel='stylesheet' href='theme/theme.css'>
 </head>
 <body>
<!--

> {-# LANGUAGE CPP #-}

-->
<p><a href="index.html">Back to Table of Contents</a></p>
<h1><a name="hello_world">Hello, World!</a></h1>

<h2><a name="first_app">Your first app!</a></h2>

<p>Our first happstack application is a simple server that responds to
all requests with the string, <kbd>Hello, World!</kbd>.</p>
<div class="code">

> module Main where
>
> import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
>
> main :: IO ()
> main = simpleHTTP nullConf $ ok "Hello, World!"

</div>
<p class="source-code">[Source code for the app is <a href="HelloWorld.hs">here.</a>]</p>

<p>To build the application run:</p>

<div class="code">
 $ ghc --make -threaded HelloWorld.hs -o helloworld
</div>

<p>The executable will be named <kbd>helloworld</kbd>.</p>

<p>Alternatively, you can use <kbd>runhaskell</kbd> and avoid the compilation step.</p>

<div class="code">
 $ runhaskell HelloWorld.hs
</div>

<p>Run this app and point your browser at <a
href="http://localhost:8000/">http://localhost:8000/</a>. (assuming
you are building the program on your local machine.)</p>

<p>If we point <kbd>curl</kbd> at the app we get the following output:</p>

<div class="code"><pre>
  $ curl http://localhost:8000/
 Hello, World!
</pre></div>

<h2><a name="how_it_works">How it works</a></h2>

<h3><a name="listening">Listening for HTTP requests</a></h3>

<p>The top-level function <code>simpleHTTP</code> is what actually starts the program listening for incoming HTTP requests.</p>
<div class="code">
#ifdef HsColour
> simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
#endif
</div>

<h3><a name="configuring">Configuring the HTTP listener</a></h3>

<p>The first argument is some simple server configuration information. It is defined as:</p>
<div class="code">
#ifdef HsColour
> data Conf = Conf { port       :: Int
>                  , validator  :: Maybe (Response -> IO Response) 
>                  , logAccess  :: forall t. FormatTime t => 
>                       Maybe (String -> String -> t -> String -> Int -> 
>                              Integer -> String -> String -> IO ())
>                  , timeout    :: Int
>                  }
#endif
</div>

<dl>
 <dt><code>port</code></dt>
 <dd>the TCP port to listen on for incoming connection</dd>
 <dt><code>validator</code></dt>   
 <dd>on-the-fly validation of output during development</dd> 
 <dt><code>logAccess</code></dt>
 <dd>logging function for HTTP requests</dd>
 <dt><code>timeout</code></dt>
 <dd>number of seconds to wait before killing an inactive connection</dd>
</dl>

<p>The default config is <code>nullConf</code> which is simply defined as:</p>
<div class="code">
#ifdef HsColour
> -- | Default configuration contains no validator and the port is set to 8000
> nullConf :: Conf
> nullConf = Conf { port      = 8000
>                 , validator  = Nothing
>                 , logAccess = Just logMAccess
>                 }
#endif
</div>

<h3><a name="processing">Processing a <code>Request</code></a></h3>

<p>The second argument is a bit more interesting. The <code>ServerPartT IO a</code> is the code that actually processes an incoming HTTP <code>Request</code> and generates a <code>Response</code>. The <code>ServerPartT IO</code> monad is essentially a fancy way of writing a function with the type:</p>

<div class="code">
#ifdef HsColour
> Request -> IO Response
#endif
</div>

<p><code>simpleHTTP</code> processes each incoming request in its own thread. It will parse the <code>Request</code>, call your <code>ServerPartT</code> handler, and then return the <code>Response</code> to the client. When developing your server part, it is natural to think about things as if you are writing a program which processes a single <code>Request</code>, generates a <code>Response</code>, and exits. However it is important when doing I/O, such as writing files to disk, or talking to a database to remember that there may be other threads running simultaneously.</p>

<h3><a name="response_code">Setting the HTTP response code</a></h3>

<p>In this example, our server part is simply:</p>
<div class="code">
#ifdef HsColour
> ok "Hello, World!"
#endif
</div>
<p><code>ok</code> is one of several combinators which can be used to set the HTTP response code. In this case, it will set the response code to <code>200 OK</code>. <a href="http://happstack.com/docs/0.5.0/happstack-server/Happstack-Server-SimpleHTTP.html"><code>Happstack.Server.SimpleHTTP</code></a> contains similar functions for the common HTTP response codes including, <code>notFound</code>, <code>seeOther</code>, <code>badRequest</code> and more. These functions all act like the normal <code>return</code> function, except they also set the response code.</p> 

<h3><a name="creating_response">Creating a <code>Response</code></a></h3>

<p>The body of the response will be "Hello, World!".</p>

<p>The <code>String</code> "Hello, World!" is turned into a <code>Response</code> because simpleHTTP calls <code>toResponse</code> from the <code>ToMessage</code> class on it. Often times we will opt to make this call explicit rather than implicit. For example:</p>
<div class="code">
#ifdef HsColour
> main :: IO ()
> main = simpleHTTP nullConf $ ok (toResponse "Hello, World!")
#endif
</div>

<p>The <code>toResponse</code> function takes care of setting the <code>Content-type</code> and converting the value into a lazy <code>ByteString</code> which will be sent back to the client. Happstack comes with pre-defined <code>ToMessage</code> instances for many types such as <code>Text.Html.Html</code>, <code>Text.XHtml.Html</code>, <code>String</code>, the types from HSP, and more.</p>


<p><a href="RouteFilters.html">Next: routing incoming requests</a></p>
<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>