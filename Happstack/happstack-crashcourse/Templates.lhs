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
<h1><a name="templates">Templating for HTML and Javascript</a></h1>

<p>Happstack supports a number of third party templating and HTML
libraries. It is easy to add support for additional libraries, if your
favorite does not already have support.</p>

<p>Each templating system has it's own set of advantages and drawbacks.</p>

<dl>
 <dt>BlazeHtml</dt>
   <dd><p>The BlazeHtml library provides combinators for generating HTML 4 and HTML 5 in Haskell.</p>
    <p>pros:</p>
     <ul>
      <li>Claims to be fast (some benchmarks to back this up)</li>
      <li>Use of combinators ensures output is always well-formed and free of typos in the names of elements and attributes</li>
      <li>Automatic escaping of String values</li>
      <li>Able to use the power of Haskell in your templates</li>
      <li>Type-checked at compile time to ensure no template values are missing</li>
      <li>Nice syntax (compared to the old html and xhtml libraries.)</li>
     </ul>
    <p>cons:</p>
     <ul>
      <li>Requires you to recompile in order to update the template</li>
      <li>Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)</li>
      <li>Only suitable for generating HTML documents</li>
      <li>Not ideal for having templates written by a web designer who does not know Haskell</li>
      <li>No compile-time assurances that generated html/xml is valid (though it will be well-formed).</li>
      <li>The <code>Html</code> monad is not a real monad, nor is it a monad transformer. This eliminates some advantage usage possibilities.</li>
     </ul>
   </dd>

 <dt>HSP</dt>
   <dd><p>HSP allows you to embed literal XML syntax inside your Haskell code. A pre-processor rewrites the literal XML into normal haskell function calls, and then the code is compiled.</p>
    <p>pros:</p>
     <ul>
      <li>Templates are compiled, so they are pretty fast (needs more benchmarks to support this statement however)</li>
      <li>You have the full power of Haskell in your templates, because it is Haskell (with a purely syntactic extension)</li>
      <li>Type-checked at compile time to ensure types are correct and no template values are missing</li>
      <li>Automatic escaping of String values</li>
      <li>Syntax is very similar to XML/HTML, so it is easy to learn</li>
      <li>Can be easier to work with when trying to populate a template from a complex Haskell type     
      <li>Can be used to generate HTML or XML</li>
     </ul>
    <p>cons:</p>
     <ul>
      <li>Requires you to recompile in order to update the template</li>
      <li>Error messages are sometimes misleading or hard to understand</li>
      <li>Makes it easy to mix the application logic and view code together, making it hard to update later (therefore you must have self control)</li>
      <li>Only suitable for generating XML and HTML documents</li>
      <li>Not ideal for having templates written by a web designer who does not know Haskell (although the xml syntax helps)</li>
      <li>No compile-time assurances that generated html/xml is valid (though it will be well-formed).</li>
     </ul>
   </dd>
  <dt>Hamlet</dt><dd></dd>
  <dt>HStringTemplate</dt><dd></dd>
  <dt>Heist</dt><dd></dd>
  <dt>XSLT</dt><dd></dd>
  <dt>more to come..</dt><dd></dd>
</dl>

#include "HelloBlaze.lhs"
#include "TemplatesHSP.lhs"
#include "TemplatesHSPI18n.lhs"
#include "TemplatesHeist.lhs"
#include "JMacro.lhs"

<p><a href="RqData.html">Next: Request parameters and data</a></p>
<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>