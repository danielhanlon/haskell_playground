<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="hello-hsp">Using HSX/HSP</a></h2>
<p class="warning">To enable HSX support, you must install the happstack-hsp package.</p>

HSX is an XML-based templating system that allows you to embed XML in your Haskell source files. If you have ever had to use PHP, you may want to run screaming from this idea. However, the HSX solution is far saner than the PHP solution, so you may want to give it a chance.

The first thing we will see is a funny `OPTIONS_GHC` pragma at the top of our file:

<div class="code">

> {-# LANGUAGE FlexibleContexts, OverlappingInstances #-}
> {-# OPTIONS_GHC -F -pgmF trhsx #-}
> module Main where
>

</div>

HSX works by running the code through an external pre-processor named `trhsx`. This pragma at the top is how we tell GHC that this file needs to be run through the `trhsx` pre-processor in order to work. So, that options line looks a bit like line noise. You can try to remember it like this:

 1. `-F` says we want to filter the source code (or maybe trans*F*orm the source code)
 2. `-pgmF` specifies the program we want to do the transformation
 3. `trhsx` is short for *tr*ansform using *hsx*

Next we have some imports:

<div class="code">

> import Control.Applicative ((<$>))
> import Control.Monad.Identity (Identity(runIdentity))
> import Data.String (IsString(fromString))
> import Data.Text   (Text)
> import qualified HSX.XMLGenerator as HSX
> import Happstack.Server.HSP.HTML
> import Happstack.Server (Request(rqMethod), ServerPartT, askRq, nullConf, simpleHTTP)
> import HSP.Identity () -- instance (XMLGen Identity)
>

</div>

Now we can define a function which generates an HTML page:

<div class="code">

> hello :: ServerPartT IO XML
> hello = unXMLGenT
>   <html>
>    <head>
>     <title>Hello, HSP!</title>
>    </head>
>    <body>
>     <h1>Hello HSP!</h1>
>     <p>We can insert Haskell expression such as this: <% sum [1 .. (10 :: Int)] %></p>
>     <p>We can use the ServerPartT monad too. Your request method was: <% getMethod %></p>
>     <hr/>
>     <p>We don't have to escape & or >. Isn't that nice?</p>
>     <p>If we want <% "<" %> then we have to do something funny.</p>
>     <p>But we don't have to worry about escaping <% "<p>a string like this</p>" %></p>
>     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
>    </body>
>   </html>
>       where
>       getMethod :: XMLGenT (ServerPartT IO) String
>       getMethod = show . rqMethod <$> askRq
>

> main :: IO ()
> main = simpleHTTP nullConf $ hello
>

</div>

The first thing we notice is that syntax looks pretty much like normal HTML syntax. There are a few key differences though:

  1. like XML, all tags must be closed
  2. like XML, we can use shortags (e.g. &lt; hr /&gt;)
  3. We do not have to escape & and >
  4. To embed < we have to do something extra funny

The syntax:

<div class="code">
&lt;% <i>haskell expression</i> %&gt;
</div>

allows us to embed a Haskell expression inside of literal XML.

As shown in this line:

<div class="code">
#ifdef HsColour
>     <p>We can also nest <% <span>like <% "this." %> </span> %></p>
#endif
</div>

we can freely nest Haskell and XML expressions.

<h3><a name="hsp-trhsx">What does <kbd>trhsx</kbd> do?</a></h3>

In order to use HSX it is very useful to understand what is actually
going on behind the magic. If we have the line:

<div class="code">
#ifdef HsColour
> foo :: XMLGenT (ServerPartT IO) XML
> foo = <span class="bar">foo</span>
#endif
</div>

and we run `trhsx`, it gets turned into a line like this:

<div class="code">

> foo :: XMLGenT (ServerPartT IO) XML
> foo = genElement (Nothing, "span") [ asAttr ("class" := "bar") ] [asChild ("foo")]
>

</div>

We see that the XML syntax has simply been translated into normal haskell function calls. 

<h3><a name="hsp-types-and-class">Important <code>HSX</code> types and classes</a></h3>

There are a few types and classes that you will need to be familiar with.

<h4><a name="hsp-xmlgent">the <code>XMLGenT</code> type</a></h4>

The first type is the `XMLGenT` monad transformer:

<div class="code">
#ifdef HsColour
> newtype XMLGenT m a = XMLGenT (m a)

> -- | un-lift.
> unXMLGenT :: XMLGenT m a -> m a
> unXMLGenT (XMLGenT ma) =  ma

#endif
</div>

This seemingly useless type exists solely to make the type-checker happy. Without it we would need an instance like:

<div class="code">
#ifdef HsColour
> instance (EmbedAsChild (IdentityT m) a, Functor m, Monad m, m ~ n) => 
>          EmbedAsChild (IdentityT m) (n a) where
>   asChild = ...
#endif
</div>

Unfortunately, because `(n a)` is so vague, that results in overlapping instances that cannot be resolved without `IncohorentInstances`. And, in my experience, enabling `IncohorentInstances` is *never* the right solution.

So, when generating XML you will generally need to apply `unXMLGenT` to the result to remove the `XMLGenT` wrapper as we did in the `hello` function. Anyone who can figure out to do away with the `XMLGenT` class will be my personal hero.

<h4><a name="hsp-xmlgen">the <code>XMLGen</code> class</a></h4>

Next we have the `XMLGen` class:

<div class="code">
#ifdef HsColour
> class Monad m => XMLGen m where
>  type XML       m
>  data Child     m
>  data Attribute m
>  genElement    :: Name
>                -> [XMLGenT m [Attribute m]]
>                -> [XMLGenT m [Child m]]
>                -> XMLGenT m (XML m)
>  genEElement   :: Name
>                -> [XMLGenT m [Attribute m]]
>                -> XMLGenT m (XML m)
>  genEElement n ats = genElement n ats []
>  xmlToChild    :: XML m -> Child m
>  pcdataToChild :: String -> Child m
#endif
</div>

Most of these functions and types are used internally and not used directly by the developer.

You will notice that we have a type-class instead of just simple functions and types. One feature of HSX is that it is not tied to any particular XML representation. Instead, the XML representation is based on the monad we are currently inside. For example, inside of a javascript monad, we might generate javascript code that renders the XML, inside of another monad, we might generate the `Node` type used by the `heist` template library. We will see some examples of this in a later section.

The `data` and `type` declarations appearing inside the class declaration are allowed because of the `TypeFamilies` extension. For a detailed coverage of type families see <a href="http://www.haskell.org/haskellwiki/GHC/Type_families">this wiki entry.</a>

<h4><a name="hsp-xml-ts">the <code>XML m</code> type synonym</a></h4>

The `XMLGen` type-class defines an associated type synonym `XML m`:

<div class="code">
#ifdef HsColour
> type XML m
#endif
</div>

`XML m` is a synonym for whatever the xml type is for the monad `m`. We can write an XML fragment that is parameterized over an arbitrary monad and xml type like this:

<div class="code">

> bar :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
> bar = <span>bar</span>
>

</div>

Note that we had this qualified import:

<div class="code">
#ifdef HsColour
> import qualified HSX.XMLGenerator as HSX
#endif
</div>

That is because we need to differentiate the `XML` associated type synonym from the plain-old `XML` data type that is declared elsewhere. Having two types with the same name is a bit silly, but that is the way it is for now.

<h4><a name="hsp-EmbedAsChild">the <code>EmbedAsChild</code> class</a></h4>

The `EmbedAsChild` is used to turn a value into a list of children of an element:

<div class="code">
#ifdef HsColour
> type GenChildList m     = XMLGenT m [Child m]
>
> -- | Embed values as child nodes of an XML element. The parent type will be clear
> -- from the context so it is not mentioned.
> class XMLGen m => EmbedAsChild m c where
>  asChild :: c -> GenChildList m
#endif
</div>

There are generally many instances of `EmbedAsChild` allowing you to embed `String`, `Text`, `Int`, and other values. You might find it useful to create additional instances for types in your program. We will some some examples later in this tutorial.

To use the `EmbedAsChild` class we us the `<% %>` syntax shown earlier. For example, when we write:

<div class="code">

> a :: (XMLGenerator m) => GenChildList m
> a = <% 'a' %>
>

</div>

It gets turned into:

<div class="code">
#ifdef HsColour
> a :: (XMLGenerator m) => GenChildList m
> a = (asChild ('a'))
>
#endif
</div>

<h4><a name="hsp-EmbedAsAttr">the <code>EmbedAsAttr</code> class</a></h4>

The `EmbedAsAttr` class is similar to the `EmbedAsChild` class. It is used to turn arbitrary values into element attributes.

<div class="code">
#ifdef HsColour
> type GenAttributeList m = XMLGenT m [Attribute m]
>
> -- | Similarly embed values as attributes of an XML element.
> class XMLGen m => EmbedAsAttr m a where
>  asAttr :: a -> GenAttributeList m
#endif 
</div>

If we have some attributes like this:

<div class="code">
#ifdef HsColour
> foo = <span class="foo" size=(80 :: Int) bogus=False>foo</span>
#endif 
</div>

It will get translated to:

<div class="code">
#ifdef HsColour
> foo
>  = (genElement (Nothing, "span")
>       [asAttr ("class" := "foo"), asAttr ("size" := (80 :: Int)),
>        asAttr ("bogus" := False)]
>       [asChild ("foo")])
#endif
</div>

which might be rendered as:

<div class="code">
&lt;span class="foo" size="80" bogus="false"
&gt;foo&lt;/span
&gt;
</div>

<h4><a name="hsp-xmlgenerator">the <code>XMLGenerator</code> class</a></h4>

You may have noticed that some of the examples had a class constraint `(XMLGenerator m)`:

<div class="code">
#ifdef HsColour
> bar :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
> bar = <span>bar</span>
>
#endif
</div>

`XMLGenerator` is just a class alias. It is defined as such:

<div class="code">
#ifdef HsColour
> class ( XMLGen m
>       , SetAttr      m (HSX.XML m)
>       , AppendChild  m (HSX.XML m)
>       , EmbedAsChild m (HSX.XML m)
>       , EmbedAsChild m [HSX.XML m]
>       , EmbedAsChild m String
>       , EmbedAsChild m Char
>       , EmbedAsAttr  m (Attr String String)
>       , EmbedAsAttr  m (Attr String Int)
>       , EmbedAsAttr  m (Attr String Bool)
>       ) => XMLGenerator m
#endif
</div>

It contains a list of common instances that all xml generation monads are expected to provide. It just saves you from having to list all thoses instances by hand when you use them.

<h3><a name="hsx-by-example">HSX by Example</a></h3>

First we have a simple function to render the pages and print them to stdout:

<div class="code">

> printXML :: Identity XML -> IO ()
> printXML = putStrLn . renderAsHTML . runIdentity

</div>

<h4><a name="hsx-and-do-syntax">HSX and <code>do</code> syntax</a></h4>

It is possible to use hsx markup inside a `do`-block. If you are using an older version of hsx, you just need to be aware of one little catch. In this example:

<div class="code">
#ifdef HsColour
> doBlock :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
> doBlock =
>     do <div>
>         <p>A child element</p>
>         </div>
#endif
</div>

Notice that we indent the closing &lt;/div&gt; tag. That indentation rule is consistent with the specification for how do-notation works. It is intend for the same reason that `if .. then .. else ..` blocks have to be idented in a special way inside `do`-blocks.

In newer versions of HSX, this restriction has been lifted.

<h4><a name="hsx-default-template"><code>defaultTemplate</code></a></h4>

There is a bit of boiler plate that appears in ever html document such as the &lt;html&gt;, &lt;head&gt;, &lt;title&gt;, and &lt;body&gt; tags. The `defaultTemplate` function provides a minimal skeleton template with those tags:

<div class="code">
#ifdef HsColour
> defaultTemplate :: ( XMLGenerator m
>                    , EmbedAsChild m body
>                    , EmbedAsChild m headers
>                    ) =>
>                    String   -- string to put in <title>
>                 -> headers  -- additional elements to put in <head>
>                 -> body     -- elements to put in <body>
>                 -> m (HSX.XML m)
#endif
</div>

<h4><a name="hsx-nothing">How to embed empty/nothing/zero</a></h4>

`defaultTemplate` requires that we pass in `headers` and a `body`. But what if we don't have any headers that we want to add?

Most `XMLGenerator` monads provide an `EmbedAsChild m ()` instance, such as this one:

<div class="code">
#ifdef HsColour
> instance EmbedAsChild Identity () where
>  asChild () = return []
#endif
</div>

So, we can just pass in `()` like so:

<div class="code">

> empty :: IO ()
> empty = printXML $ defaultTemplate "empty" () ()

</div>

Which will render as such:

<div class="code">
<pre>
&lt;html
&gt;&lt;head
  &gt;&lt;title
    &gt;empty&lt;/title
    &gt;&lt;/head
  &gt;&lt;body
  &gt;&lt;/body
  &gt;&lt;/html
&gt;
</pre>
</div>


<h4><a name="hsx-list-of-children">Creating a list of children</a></h4>

Sometimes we want to create a number of child elements without knowing what their parent element will be. We can do that using the:

<div class="code">

&lt;%&gt; ... &lt;/%&gt;

</div>

syntax. For example, here we return two paragraphs:

<div class="code">

> twoParagraphs :: (XMLGenerator m) => XMLGenT m [HSX.Child m]
> twoParagraphs =
>     <%>
>      <p>Paragraph one</p>
>      <p>Paragraph two</p>
>    </%>

</div>

We can embed those in parent element like this:

<div class="code">

> twoParagraphsWithParent :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
> twoParagraphsWithParent =
>     <div>
>      <% twoParagraphs %>
>     </div>

</div>

<h4><a name="hsx-if-then-else"><code>if .. then .. else .. </code></a></h4>

Using an `if .. then .. else ..` is straight-foward. But what happens
when you don't really want an `else` case? This is another place we
can use `()`:

<div class="code">

> ifThen :: Bool -> IO ()
> ifThen bool =
>     printXML $ defaultTemplate "ifThen" () $
>      <div>
>       <% if bool 
>          then <%
>                <p>Showing this thing.</p> 
>               %>
>          else <% () %>
>        %>
>      </div>
>

</div>

<h4><a name="hsx-attr-list">Lists of attributes &amp; optional attributes</a></h4>

Normally attributes are added to an element using the normal html attribute syntax. HSX, has a special extension where the last attribute can be a Haskell expression which returns a list of attributes to add to the element. For example:

<div class="code">

> attrList :: IO ()
> attrList = 
>     printXML $ defaultTemplate "attrList" () $
>      <div id="somediv" ["class" := "classy", "title" := "untitled"] >
>      </div>
>

</div>

The type of the elements of the list can be anything with an `EmbedAsAttr m a` instance. In this case we create a list of `Attr` values:

<div class="code">
#ifdef HsColour
> data Attr n a = n := a
#endif
</div>

We can use this feature to conditionally add attributes using a simple `if .. then .. else ..` statment:

<div class="code">

> optAttrList :: Bool -> IO ()
> optAttrList bool = 
>     printXML $ defaultTemplate "attrList" () $
>      <div id="somediv" (if bool
>                           then ["class" := "classy", "title" := "untitled"]
>                           else []) >
>      </div>
>

</div>

<p class="source-code">[Source code for the app is <a href="TemplatesHSP.hs">here.</a>]</p>

<h3><a name="hsp-trouble">HSX and compilation errors</a></h3>

One drawback to HSX is that it can result in some pretty ugly (and sometimes very long) error messages. Fortunately, the errors are almost always the same type of thing, so after a little experience it is easy to see what is going wrong. Here are some tips if you run into errors:

<h4><a name="hsp-line-numbers">Line numbers are usually wrong</a></h4>

As we saw, <kbd>trhsx</kbd> transforms the literal XML into normal Haskell code. Unfortunately, the error positions reported by GHC reflect where the error occurred in the transformed code, not the original input. HSX tries to help GHC by inserting LINE pragmas. While that helps to a degree, it still leaves a fair bit of fuzz. 

The trick is to look towards the bottom of the error message where it will usually show you the expression that contained the error. For example, if we have:

<div class="code">
#ifdef HsColour
> typeError :: XMLGenT (ServerPartT IO) XML
> typeError = <foo><% 1 + 'a' %></foo>
#endif
</div>

We will get an error like:

<div class="code">
<pre>
TemplatesHSP.markdown.lhs:459:59:
    No instance for (Num Char)
      arising from a use of `+'
    Possible fix: add an instance declaration for (Num Char)
    In the first argument of `asChild', namely `(1 + 'a')'
    In the first argument of `asChild', namely `((asChild (1 + 'a')))'
    In the expression: asChild ((asChild (1 + 'a')))
</pre>
</div>

The last line says:

<div class="code">
<pre>
    In the expression: asChild ((asChild (1 + 'a')))
</pre>
</div>

which is, indeed, where the type error is.

A bug report about the line number issue has been filed, and there are ideas on how to fix it. You can <a href="http://groups.google.com/group/haskell-server-pages/browse_thread/thread/1b136c7acb448136">read more here.</a>


<h4><a name="hsp-line-numbers">Overlapping Instances</a></h4>

Another common error is that of overlapping instances. For example, if we wrote the following:

<div class="code">
#ifdef HsColour
> overlapping = <p>overlapping</p>
#endif
</div>

We would get an error like:

<div class="code">
<pre>
TemplatesHSP.markdown.lhs:495:36:
    Overlapping instances for EmbedAsChild m0 [Char]
      arising from a use of `asChild'
    Matching instances:
      instance [overlap ok] HSX.XMLGen m => EmbedAsChild m String
        -- Defined in `HSX.XMLGenerator'
      instance EmbedAsChild Identity String -- Defined in `HSP.Identity'
      instance Monad m => EmbedAsChild (ServerPartT m) String
        -- Defined in `HSP.ServerPartT'
    (The choice depends on the instantiation of `m0'
     To pick the first instance above, use -XIncoherentInstances
     when compiling the other instance declarations)
    In the expression: asChild ("overlapping")
    In the third argument of `genElement', namely
      `[asChild ("overlapping")]'
    In the expression:
      (genElement (Nothing, "p") [] [asChild ("overlapping")])
</pre>
</div>

I have never enabled `IncoherentInstances` and actually had it do what
I wanted. In this case, the solution is to add an explicit type
signature that mentions the missing constraint:

<div class="code">

> overlapping :: (EmbedAsChild m String) => XMLGenT m (HSX.XML m)
> overlapping = <p>overlapping</p>

</div>

In general, there can be a lot of required `EmbedAsChild` and
`EmbedAsAttr` instances. So, often times you can save a lot of typing
by using the `XMLGenerator` class alias:

<div class="code">

> overlapping' :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
> overlapping' = <p>overlapping</p>

</div>

<h4><a name="hsp-line-ambiguous-types">Ambiguous Types</a></h4>

Sometimes a type signature for the parent function is not enough. For example, let's say we have:

<div class="code">
#ifdef HsColour
> ambiguous :: (EmbedAsChild m String) => XMLGenT m (HSX.XML m)
> ambiguous = <p><% fromString "ambiguous" %></p>
#endif
</div>

That will generate an error like this one:

<div class="code">
<pre>
TemplatesHSP.markdown.lhs:557:28:
    Ambiguous type variable `c0' in the constraints:
      (IsString c0)
        arising from a use of `fromString'
        at TemplatesHSP.markdown.lhs:557:28-37
      (EmbedAsChild m c0)
        arising from a use of `asChild'
        at TemplatesHSP.markdown.lhs:557:19-25
    Probable fix: add a type signature that fixes these type variable(s)
    In the first argument of `asChild', namely
      `(fromString "ambiguous")'
    In the first argument of `asChild', namely
      `((asChild (fromString "ambiguous")))'
    In the expression: asChild ((asChild (fromString "ambiguous")))
Failed, modules loaded: none.
</pre>
</div>

Here we are trying to use `fromString` to convert `"ambiguous"` into some type, and then we embed that type using `asChild`. But there is not enough information to figure out what the intermediate type should be. It is the same problem we have if we try to write:

<div class="code">
#ifdef HsColour
> \str -> show (read str)
#endif
</div>

The solution here is to add an explicit type signature to the result of `fromString`:

<div class="code">

> ambiguous :: (EmbedAsChild m Text) => XMLGenT m (HSX.XML m)
> ambiguous = <p><% (fromString "ambiguous") :: Text %></p>

</div>

<h4><a name="hsp-line-overloaded-strings">HSX + <code>OverloadedStrings</code></a></h4>

Unfortunately, HSX and the `OverloadedStrings` extension do not play together nicely.  If we were to write:

<div class="code">

> overloaded :: XMLGenT Identity XML
> overloaded = <p>Hello</p>

</div>

That would be transformed into:

<div class="code">
#ifdef HsColour
> overloaded = (genElement (Nothing, "p") [] [asChild ("Hello")])
#endif
</div>

However, with the `OverloadStrings` extension enabled, the compiler will automatically insert `fromString` in front of every string literal like this:

<div class="code">
#ifdef HsColour
> overloaded = (genElement (Nothing, fromString "p") [] [asChild (fromString "Hello")])
#endif
</div>

Which results in the ambiguous error as we saw in the last section.

The best workaround for this at the moment is to not use `OverloadedStrings` and `HSX` in the same module. If you really want `OverloadedStrings` in some of your code, then you might want to put all your HSX templates in a separate module that does not use the extension. Aside from working around the issue, it can also help to improve your code structure. Many people frown on mixing the presentation and business logic layers. Putting your templates in separate modules can help keep the presentation and business layer separate.

Finding a better solution is still an open problem which has not been examined yet. It would be great if HSX played nicely with `OverloadedStrings`. It would also be nice to create an XML type for use with HSX that was based around `Text`. These two things are a bit intertwined since you generally want to use `OverloadedStrings` when using `Text`.

