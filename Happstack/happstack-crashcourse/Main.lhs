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

> {-# OPTIONS_GHC -cpp #-}

-->
<h1><a name="happstack_crashcourse">Happstack Crashcourse</a></h1>

<p>Happstack is the <b>H</b>askell <b>app</b>lication server
<b>stack</b>. It is primarily used for web application development,
but it can be used for any type of application server.</p>

<p>Happstack's mission is to enable developers to prototype quickly,
deploy painlessly, scale massively, operate reliably, and change
easily.</p>

<h2>Why You Should Read This</h2>

<p>This guide is designed to provide you with a clear, practical
understanding of how to develop applications with Happstack. It is not
especially witty or entertaining, but it should be very easy to read
and pratical to apply. I have avoided any talk of theory and just
focused on how to get things done.</p>

<p>Every topic covered in the course comes with a small,
self-contained, fully-functional example which you can download directly
from the page. This should make it very easy to experiment and to
<i>learn by doing</i>.</p>

<p>I highly recommend you read this tutorial first rather than trying
to understand Happstack by looking at the source code and haddock
documentation. Learning Happstack by looking at the source and haddock
docs would be like trying to learn to drive by first opening up the
hood of the car and trying to understand what is going and then
reading the owners manual.</p>

<p>This guide will take you through all the practical aspects of using
Happstack, and keep you from accidentally focusing on unimportant
things like <code>UnWebT</code>. This is a course for people who want
to drive the car, not open an auto shop.</p>

<p>This tutorial assumes that you will be deploying your application
on a VPS or dedicated server where you can have a server process that
is always running. However, many components of Happstack can be used
in a CGI / shared-server environment.</p>

<p>This tutorial is still a work in progress. If you want to contribute to this tutorial you can get the source via:</p>

<p><kbd>darcs get http://src.seereason.com/happstack-crashcourse</kbd></p>

<p>If you find mistakes or have difficulty following portions of this course, please let us know on the <a href="http://groups.google.com/group/HAppS?pli=1">mailing list</a> so that we can make fixes and improvements.</p>

#include "toc.html"

 <script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>