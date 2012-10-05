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
<h1><a name="acid_state"><kbd>acid-state</kbd></a></h1>

<p><kbd>acid-state</kbd> is a NoSQL, RAM-cloud, persistent data store. One of attractive feature is that it's designed to store arbitrary Haskell datatypes and queries are written using plain old Haskell code. This means you do not have to learn a special query language, or figure out how to turn your beautiful Haskell datastructures into some limited set of ints and strings.</p>

<p><kbd>acid-state</kbd> and <kbd>safecopy</kbd> are the successors to the old <kbd>happstack-state</kbd> and <kbd>happstack-data</kbd> libraries. You can learn more at the <a href="http://acid-state.seize.it/">acid-state homepage</a>. acid-state is now completely independent from Happstack and can be used with any web framework. However, Happstack is still committed to the improvement and promotion of <kbd>acid-state</kbd>.</p>

<p>Apps written using <kbd>happstack-state</kbd> can be migrated to use <kbd>acid-state</kbd> relatively easily. Details on the process or documented <a href="http://code.google.com/p/happstack/wiki/HapstackStateToAcidState">here</a>.

<h2><a name="acid_state_how_it_works">How <kbd>acid-state</kbd> works</a></h2>

<p>A very simple way to model a database in Haskell would be to create a datatype to represent your data and then store that data in a mutable, global variable, such as a global <code>IORef</code>. Then you could just write normal Haskell functions to query that value and update it. No need to learn a special query language. No need to marshal your types from expressive Haskell datatypes to some limited set of types supported by an external database.</p>

<p>That works great.. as long as your application is only single-threaded, and as long as it never crashes, and never needs to be restarted. But, for a web application, those requires are completely unacceptable. The idea is still appealing though. <kbd>acid-state</kbd> provides a practical implementation of that idea which actually implements the ACID guarantees that you may be familiar with from traditional relational databases such as MySQL, postgres, etc.</p>

<p>In <code>acid-state</code> we start by defining a type that represents the state we wish to store. Then we write a bunch of pure functions that query that value or which return an updated value. However, we do not call those functions directly. Instead we keep the value inside an <code>AcidState</code> handle, and we call our functions indirectly by using the <code>update</code> and <code>query</code> functions. This allows <kbd>acid-state</kbd> to transparently log update events to disk, to ensure that update and query events run automatically and in isolation, etc. It is allows us to make remote API calls, and, eventually, replication and multimaster.</p>

<p>Note that when we say <kbd>acid-state</kbd> is pure, we are referring specifically to the fact that the functions we write to perform updates and queries are pure. <kbd>acid-state</kbd> itself must do IO in order to coordinate events from multiple threads, log events to disk, perform remote queries, etc.</p>

<p>Now that you have a vague idea how <kbd>acid-state</kbd> works, let's clarify it by looking at some examples.</p>

#include "AcidStateCounter.lhs"
#include "IxSet.lhs"
#include "IxSetDataLens.lhs"
#include "AcidStateAdvanced.lhs"

<p><a href="TemplateHaskell.html">Next: Template Haskell</a></p>

<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>