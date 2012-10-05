<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" 
          "http://www.w3.org/TR/html4/strict.dtd">
<html>
 <head>
  <title>Crash Course in Happstack</title>
  <link type='text/css' rel='stylesheet' href='hscolour.css'>
  <link type='text/css' rel='stylesheet' href='theme/theme.css'>
 </head>
 <body>
<!--

> {-# LANGUAGE CPP #-}

-->
<p><a href="index.html">Back to Table of Contents</a></p>
<h1><a name="route_filters">Route Filters</a></h1>
<p><i>a.k.a Responding to different url paths</i></p>

<p>Happstack provides a variety of ways to match on parts of the <code>Request</code> (such as the path or request method) and respond appropriately.</p>

<p>Happstack provides two different systems for mapping the request path to a handler. In this section we will cover a simple, untyped routing system. <a href="WebRoutes.html">Later in the crash course</a> we will look at fancier, type-safe routing sytem known as <kbd>web-routes</kbd>.</p>

#include "MonadPlus.lhs"

<h2><a name="static_path">Matching on static path segments</a></h2>

#include "Dir.lhs"
#include "Dir2.lhs"
#include "Dirs.lhs"
#include "Path.lhs"
#include "FromReqURI.lhs"
#include "MethodM.lhs"
#include "MatchMethod.lhs"

<h2><a name="other_filters">Other Routing Filters</a></h2>

<p>SimpleHTTP includes a number of other useful routing filters, such as:</p>
<dl>
 <dt><code>nullDir :: (ServerMonad m, MonadPlus m) => m ()</code></dt>
  <dd>check that there are no path segments remaining</dd>

 <dt><code>host :: (ServerMonad m, MonadPlus m) => String -> m a -> m a</code></dt>
  <dd>match on a specific host name in the Request</dd>

 <dt><code>withHost :: (ServerMonad m, MonadPlus m) => (String -> m a) -> m a</code></dt>
  <dd>Lookup the host header and pass it to the handler.</dd>

 <dt><code>uriRest :: (ServerMonad m) => (String -> m a) -> m a</code></dt>
  <dd>Grab the rest of the URL (dirs + query) and passes it to your handler</dd>

 <dt><code>anyPath :: (ServerMonad m, MonadPlus m) => m r -> m r</code></dt>
  <dd>Pop any path element and ignore when choosing a 'ServerPartT' to handle the request.</dd>

 <dt><code>trailingSlash :: (ServerMonad m, MonadPlus m) => m ()</code></dt>
   <dd>Guard which checks that the Request URI ends in <code>/</code>.  Useful for distinguishing between <code>foo</code> and <code>foo/</code></dd>
</dl>
<p><a href="Templates.html">Next: templates and HTML</a></p>
<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>