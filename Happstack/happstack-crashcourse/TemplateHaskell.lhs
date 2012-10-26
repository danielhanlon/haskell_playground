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
<h1><a name="template_haskell">Using Template Haskell</a></h1>


<p>Template Haskell is a GHC extension that makes it possible to generate new code at compile time. It is like a more powerful version of C macros, but a bit more restrictive than LISP macros. You can see the code that is being generated by passing the <kbd>-ddump-splices</kbd> flag to GHC.</p>

<p>There are only a few places in Happstack where you will encounter Template Haskell code. In each of those cases, it is used to generate some very boilerplate code. You are already familiar with one code generation mechanism in Haskell -- the <code>deriving (Eq, Ord, Read, Show, Data, Typeable)</code> clause. In Happstack, we use Template Haskell in a similar manner to derive instances of classes like <code>SafeCopy</code> and <code>IsAcidic</code>.</p>

<p>There are only a few simple things you will need to learn to use Template Haskell with Happstack.</p>

<p>To enable Template Haskell you will need to include <code>{-# LANGUAGE TemplateHaskell #-}</code> at the top of the file.</p>

<p>Here is an example of some Template Haskell that derives a <code>SafeCopy</code> instance:</p>

<div class="code">
#ifdef HsColour
> $(deriveSafeCopy 0 'base ''CounterState)
#endif
</div>

<p>There are three new pieces of syntax you will see:</p>

<dl>
 <dt><code>$( )</code></dt>
 <dd>This syntax is used to indicate that the code inside is going to generate code. The <code>$(..)</code> will be replaced by the generated code, and then the module will be compiled. The use of <code>$( )</code> is optional (since GHC 6.12 or so).</dd>
 <dt><code>'</code></dt>
 <dd>The single quote in <code>'base</code> is syntax that returns the <code>Name</code> of a function or constructor. (Specificially, <code>Language.Haskell.TH.Syntax.Name</code>).<dd>
 <dt><code>''</code></dt>
 <dd>Note: that is two single ticks <code>''</code> <i>not</i> a double-quote <code>"</code>. It serves the same purpose as <code>'</code> except that it is used to get the <code>Name</code> of a type instead of a function or constructor.</dd>
</dl>

<p>Finally, you may occasionally run into some staging restrictions. In a normal Haskell source file, it does not matter what order you declare things. You can use a type in a type signature, and then define the type later in the file. However, when using Template Haskell, you may occasionally find that you need to order your types so that they are declared before they are used. If the compiler complains that it can't find a type that you have clearly defined in the file, try moving the declaration up higher.</p>

<p>That is everything you should need to know to use Template Haskell in Happstack. See the relevant section of the crash course for the details of calling specific Template Haskell functions such as <code>deriveSafeCopy</code>.</p>

<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>