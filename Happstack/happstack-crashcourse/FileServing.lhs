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
<h1><a name="fileserving">Serving Files from Disk</a></h1>

<p>Happstack can be used to serve static files from disk, such as <kbd>.html</kbd>, <kbd>.jpg</kbd>, etc.</p>

<p>The file serving capabilities can be divided into two categories:</p>
<ol>
 <li>Serving files from a directory based on a direct mapping of a portion of the URI to file names on the disk</li>
 <li>Serving an specific, individual file on disk, whose name may be different from the URI</li>
</ol>

#include "FileServingDirectory.lhs"
#include "FileServingSingle.lhs"
#include "FileServingAdvanced.lhs"

<p><a href="Reform.html">Next: Reform</a></p>

<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>