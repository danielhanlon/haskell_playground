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
<h1><a name="web-routes">Web Routes</a></h1>

<p>The <kbd>web-routes</kbd> libraries provide a system for type-safe url routing. The basic concept behind type-safe urls is very simple. Instead of working directly with url strings, we create a type that represents all the possible urls in our web application. By using types instead of strings we benefit in several ways:</p>

<dl>
 <dt>fewer runtime errors due to typos<dt>
 <dd>If you mistype the <code>String</code>, "/hmoe" instead of "/home", the compiler will gleefully compile it. But if you mistype the constructor as <code>Hmoe</code> instead of <code>Home</code> you will get a compile time error.</dd>
 <dt>Compile type assurance that all routes are mapped to handlers</dt>
 <dd>Routing is performed via a simple <code>case</code> statement on the url type. If you forget to handle a route, the compiler will give you an <kbd>Pattern match(es) are non-exhaustive</kbd> warning.</dd>
 <dt>unique URLs for 3rd party libraries</dt>
 <dd>Libraries (such as a blog or image gallery component) need a safe way to create urls that do no overlap with the routes provided by other libraries. For example, if a blog component and image component both try to claim the url <kbd>/upload</kbd>, something bad is going to happen. With <kbd>web-routes</kbd>, libraries do not have to take any special steps to ensure that the urls they generate are unique. <kbd>web-routes</kbd> are composable and result in unique urls.</dd>
 <dt>Compile time errors when routes change</dt>
 <dd>As a website evolves, existing routes might change or be removed entirely. With <kbd>web-routes</kbd> this will result in a change to the type. As a result, code that has not been updated will generate a compile-time error, instead of a runtime error. This is especially valuable when using 3rd party libraries, since you may not even be aware that the route had changed otherwise.</dd>
 <dt>better separation of presentation and behavior</dt>
 <dd>In <kbd>web-routes</kbd>, the parsing and printing of a url is separated from the mapping of a url to a handler or creating hyperlinks in your code. This makes it trivial to change the way the url type is converted to a string and back. You need only modify the function that does the conversion, and everything else can stay the same. You do not need to hunt all over the code trying to find places that use the old format.</dd>
 <dt>self-documentation sitemap</dt>
 <dd>Because the url type represents all the valid routes on your site, it also acts as a simple sitemap.</dd>
</dl>

<p><kbd>web-routes</kbd> is designed to be very flexible. For example, it does not require that you use any particular mechanism for defining the mapping between the url type and the url string. Instead, we provide a variety of addon packages that provide different methods including, template-haskell, generics, parsec, quasi-quotation, and more. This means it is also easy to add your own custom mechanism. For example, you might still use template-haskell, but with a different set of rules for converting a type to a string.</p>

<p><kbd>web-routes</kbd> is also not limited to use with any particular framework, templating system, database, etc. In fact, <kbd>web-routes</kbd> provides the foundation for type-safe url routing in <kbd>yesod</kbd>.</p>

#include "WebRoutesDemo.lhs"
#include "WebRoutesBoomerang.lhs"
#include "WebRoutesHSP.lhs"

<h2><a name="web-routes-coming">Still to come</a></h2>
<p>I am working on additional sections which will cover creating 'sub-sites' that can be embedded into larger sites, integration with HSP, and more.</p>

<p><a href="AcidState.html">Next: Acid State</a></p>

<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>