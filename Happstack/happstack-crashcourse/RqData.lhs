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
<h1><a name="rqdata">Parsing request data from the QUERY_STRING, cookies, and request body</a></h1>

<p>The <code>RqData</code> module is used to extract key/value pairs from the QUERY_STRING, cookies, and the request body of a <code>POST</code> or <code>PUT</code> request.</p>

#include "HelloRqData.lhs"
#include "RqDataPost.lhs"
#include "RqDataUpload.lhs"
#include "RqDataLimiting.lhs"
#include "RqDataError.lhs"
#include "RqDataParsing.lhs"
<!-- include "RqDataFromData.lhs" -->
#include "RqDataOptional.lhs"
#include "Cookies.lhs"

<p><a href="FileServing.html">Next: Serving Files from Disk</a></p>

<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>