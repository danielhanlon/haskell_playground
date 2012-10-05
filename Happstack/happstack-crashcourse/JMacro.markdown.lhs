<!--

> {-# LANGUAGE CPP #-}

-->
<h2><a name="jmacro">JavaScript via JMacro</a></h2>
<p class="warning">To use JMacro with happstack and hsx, you should install the
  <kbd>hsx-jmacro</kbd> and <kbd>happstack-jmacro</kbd> packages. You will also need to be sure that your version of <kbd>happstack-hsp</kbd> is <kbd>&gt;= 6.1.0</kbd>.</p>

<a href="http://www.haskell.org/haskellwiki/Jmacro">JMacro</a> is a library that makes it easy to include javascript in your templates.

The syntax used by JMacro is almost identical to JavaScript. So, you
do not have to learn some special DSL to use it. In fact, JMacro can
work with most JavaScript you find in the wild. Using JMacro has a
number of advantages over just using plain-old javascript.

 * syntax checking ensures that your JavaScript is syntactically valid
   at compile time. That eliminates many common JavaScript errors and
   reduces development time.

 * hygienic names and scoping automatically and transparently ensure
   that blocks of JavaScript code do not accidentally create variables
   and functions with conflicting names.

 * Antiquotation, marshalling, and shared scope make it easy to splice
   Haskell values into the JavaScript code. It also makes it easy to
   programmatically generate JavaScript code.

The <kbd>hsx-jmacro</kbd> and <kbd>happstack-jmacro</kbd> libraries makes it easy to use JMacro with <a href="http://www.happstack.com/">Happstack</a> and HSP.

The following examples demonstrate the basics of JMacro and how it interfaces with HSX and Happstack. The examples are intended to demonstrate what is possible with JMacro. The examples are not intended to demonstrate good javascript practices. For example, many developers frown on the use of the <code>onclick</code> attribute in html, or having <code>&lt;script&gt;</code> tags in the <code>&lt;body&gt;</code>.

The JMacro library does not require any external
pre-processors. Instead it uses the magic of <a
href="http://haskell.org/haskellwiki/Quasiquotation">QuasiQuotation</a>.

<code>QuasiQuotes</code> can be enabled via the <code>LANGUAGE</code> extension:

<div class="code">

> {-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving,
>     TypeSynonymInstances, QuasiQuotes #-}

</div>

In this example we are also using HSX, which does require a
pre-processor. (A crash course section on HSX itself will be coming soon). The following line will automatically run the
pre-processor for us (and also suppress warnings about orphan
instances):
<div class="code">

> {-# OPTIONS_GHC -F -pgmFtrhsx -fno-warn-orphans #-}

</div>
Next we have a boatload of imports. Not all of these are required to use JMacro. Many are just used for the demos.

There is one really import thing to note though. If you look at the import for `Language.Javascript.JMacro`, you will find that there are a bunch of things imported like `jsVarTy` which we never call explicitly in this demo. The calls to these functions are generated automatically by the `JMacro` quasi-quoters. `JMacro` can not automatically add these imports, so you will need to do it by hand if you use explicit import lists. Alternatively, you can just import `Language.Javascript.JMacro` without an explicit import list.

<div class="code">

> import Control.Applicative ((<$>), optional)
> import Control.Monad       (msum)
> import Control.Monad.State (StateT, evalStateT)
> import Control.Monad.Trans (liftIO)
> import qualified Data.Map  as Map
> import Data.Maybe          (fromMaybe)
> import Happstack.Server    (Response, ServerPartT, dir, mapServerPartT, look, nullConf,
>                             ok, simpleHTTP, toResponse)
> import Happstack.Server.HSP.HTML  (defaultTemplate) -- ^ also imports 'ToMessage XML'
> import Happstack.Server.JMacro    (jmResponse)      -- ^ ToMessage instance for JStat
> import HSP                        ( Attr(..), EmbedAsAttr(..), EmbedAsChild(..)
>                                   , genElement, genEElement
>                                   )
> import HSP.ServerPartT            () -- ^ instance 'XMLGenerator ServerPartT'
> import HSX.JMacro                 ( IntegerSupply(..)
>                                   , nextInteger') -- EmbedAsChild & EmbedAsAttr for JStat
> import Language.Javascript.JMacro ( ToJExpr(..), Ident(..), JStat(..), JExpr(..)
>                                   , JVal(..), jmacro, jsv, jLam, jVarTy)
> import System.Random              (Random(..))
>

</div>

In order to ensure that each <code>&lt;script&gt;</code> tag generates unique variables
names, we need a source of unique prefixes. An easy way to do that is to
wrap the <code>ServerPartT</code> monad around a <code>StateT</code> monad that supplies
integers:
<div class="code">

> type JMacroPart = ServerPartT (StateT Integer IO)
>
> instance IntegerSupply JMacroPart where
>     nextInteger = nextInteger'
>

</div>
The <code>nextInteger'</code> helper function has the type:

<div class="code">
#ifdef HsColour
>  nextInteger' :: (MonadState Integer m) => m Integer
#endif
</div>

To use JMacroPart with <code>simpleHTTP</code>, we just evaluate the <code>StateT</code> monad:
<div class="code">

> main :: IO ()
> main = simpleHTTP nullConf $ flatten handlers
>     where
>       flatten :: JMacroPart a -> ServerPartT IO a
>       flatten = mapServerPartT (flip evalStateT 0)
>

</div>

<h3><a name="hello-jmacro">JMacro in a <code>&lt;script&gt;</code> tag</a></h3>

Now that we have the scene set, we can actually look at some JMacro usage.

In this example we embed a single JavaScript block inside the page:
<div class="code">

> helloJMacro :: JMacroPart Response
> helloJMacro =
>     toResponse <$> defaultTemplate "Hello JMacro" ()
>       <div>
>        <% [$jmacro|
>            var helloNode = document.createElement('h1');
>            helloNode.appendChild(document.createTextNode("Hello, JMacro!"));
>            document.body.appendChild(helloNode);
>            |] %>
>       </div>
>

</div>

We do not need to specify the <code>&lt;script&gt;</code> tag explicitly, it will automatically be created for us.

The syntax `[$jmacro| ... |]` is the magic incantation for running the
`jmacro` quasiquoter. In GHC 7.x, the $ is no longer required, so in
theory you could write, `[jmacro| ... |]`. However, HSX has not been updated to support the $ free syntax. So, for now you will need to stick with the $ syntax, despite the compiler warnings saying, <kbd>Warning: Deprecated syntax: quasiquotes no longer need a dollar sign: $jmacro</kbd>.

<h3><a name="hello-attr">JMacro in an HTML attribute (<code>onclick</code>, etc)</a></h3>

We can also use JMacro inside html attributes, such as <code>onclick</code>.
<div class="code">

> helloAttr :: JMacroPart Response
> helloAttr =
>  toResponse <$> defaultTemplate "Hello Attr" ()
>  <h1 style="cursor:pointer" onclick=[$jmacro| alert("that </tickles>!") |]>Click me!</h1>
>

</div>
Note that we do not have to worry about escaping the ", < or > in the
onclick handler. It is taken care of for us automatically! The code is automatically escaped as:

<div class="code">
<pre>onclick="alert(&amp;quot;that &amp;lt;/tickles&amp;gt;!&amp;quot;);"</pre>
</div>

<h3><a name="jmacro-end-tag-escaping">Automatice escaping of &lt;/</a></h3>

According to the HTML <a
href="http://www.w3.org/TR/html401/appendix/notes.html#notes-specifying-data">spec</a>
it is invalid for &lt;/ to appear anywhere inside the <code>&lt;script&gt;</code> tag.

The JMacro embedding also takes care of handling &lt;/ appearing in
string literals. So we can just write this:

<div class="code">

> helloEndTag :: JMacroPart Response
> helloEndTag =
>     toResponse <$> defaultTemplate "Hello End Tag" ()
>     <%>
>      <h1>Tricky End Tag</h1>
>      <% [$jmacro| alert("this </script> won't mess things up!") |] %>
>     </%>
>

</div>

And it will generate:

<div class="code">
<pre>&lt;script type="text/javascript"&gt;alert("this &lt;\/script&gt; won't mess things up!");&lt;/script&gt;</pre>
</div>

<h3><a name="jmacro-hygienic">Hygienic Variable Names</a></h3>

So far, using HSP with JMacro looks almost exactly like using HSP with
plain-old JavaScript. That's actually pretty exciting. It means that
the mental tax for using JMacro over straight JavaScript is very low.

Now let's look at an example of hygienic naming. Let's say we write
the following block of JavaScript code:
<div class="code">

> clickMe :: JStat
> clickMe =
>     [$jmacro|
>
>     var clickNode = document.createElement('p');
>     clickNode.appendChild(document.createTextNode("Click me!"));
>     document.body.appendChild(clickNode);
>     var clickCnt = 0;
>     clickNode.setAttribute('style', 'cursor: pointer');
>     clickNode.onclick = function () { clickCnt++;
>                                       alert ('Been clicked ' + clickCnt + ' time(s).');
>                                     };
>     |]
>

</div>

That block of code tracks how many times you have clicked on the
<kbd>Click me!</kbd> text. It uses a global variable to keep track of
the number of clicks. Normally that would spell trouble. If we tried
to use that code twice on the same page, both copies would end up
writing to the same global variable `clickCnt`.

But, JMacro automatically renames the variables for us so that the
names are unique. In the following code each <kbd>Click me!</kbd> tracks its
counts separately:
<div class="code">

> clickPart :: JMacroPart Response
> clickPart =
>     toResponse <$> defaultTemplate "Hygienic Naming" ()
>                    <div>
>                     <h1>A Demo of Happstack+HSP+JMacro</h1>
>                     <% clickMe %>
>                     <% clickMe %>
>                    </div>
>

</div>

<h3><a name="jmacro-non-hygienic">Non-Hygienic Variable Names</a></h3>

Of course, sometimes we want the code blocks to share a global
variable. We can easily do that by changing the line:
<div class="code">

#ifdef HsColour
>   var clickCnt = 0;
#endif

</div>
to
<div class="code">

#ifdef HsColour
>   var !clickCnt = 0;
#endif

</div>
The use of ! when declaring a variable disables hygienic naming. Now all the copies of <code>clickMe2</code> will share the same counter:

<div class="code">

> clickMe2Init :: JStat
> clickMe2Init =
>     [$jmacro| var !clickCnt = 0; |];
>
> clickMe2 :: JStat
> clickMe2 =
>     [$jmacro|
>
>     var clickNode = document.createElement('p');
>     clickNode.appendChild(document.createTextNode("Click me!"));
>     document.body.appendChild(clickNode);
>     clickNode.setAttribute("style", "cursor: pointer");
>     clickNode.onclick = function () { clickCnt++;
>                                       alert ('Been clicked ' + clickCnt + ' time(s).');
>                                     };
>     |]
>
> clickPart2 :: JMacroPart Response
> clickPart2 =
>     toResponse <$> defaultTemplate "Hygienic Naming"
>                    <% clickMe2Init %>
>                    <div>
>                     <h1>A Demo of Happstack+HSP+JMacro</h1>
>                     <% clickMe2 %>
>                     <% clickMe2 %>
>                    </div>
>

</div>

<h3><a name="jmacro-functions">Declaring Functions</a></h3>

Hygienic naming affects function declarations as well. If we want to define a function in <code>&lt;head&gt;</code>, but call the function from the <code>&lt;body&gt;</code>, then we need to disable hygienic naming. We can do that using the ! trick again:

<div class="code">
#ifdef HsColour
> function !hello(noun) { alert('hello ' + noun); }
#endif
</div>

JMacro also has some syntax extensions for declaring functions. We can
create an anonymous function using Haskell-like syntax assign it to a variable:

<div class="code">
#ifdef HsColour
> var !helloAgain = \noun ->alert('hello again, ' + noun);
#endif
</div>

Another option is to use the ML-like <code>fun</code> keyword to declare a function. When using <code>fun</code> we do not need the !.

<div class="code">
#ifdef HsColour
> fun goodbye noun { alert('goodbye ' + noun); }
#endif
</div>

Or we can do both:

<div class="code">
#ifdef HsColour
> fun goodbyeAgain noun -> alert('goodbye again, ' + noun);
#endif
</div>

Here they all are in an example:

<div class="code">

> functionNames :: JMacroPart Response
> functionNames =
>     toResponse <$> defaultTemplate "Function Names"
>           <% [$jmacro|
>                function !hello(noun) { alert('hello, ' + noun); }
>                var !helloAgain = \noun ->alert('hello again, ' + noun);
>                fun goodbye noun { alert('goodbye ' + noun); }
>                fun goodbyeAgain noun -> alert('goodbye again, ' + noun);
>              |]
>            %>
>           <%>
>             <button onclick=[$jmacro| hello('world'); |]>hello</button>
>             <button onclick=[$jmacro| helloAgain('world'); |]>helloAgain</button>
>             <button onclick=[$jmacro| goodbye('world'); |]>goodbye</button>
>             <button onclick=[$jmacro| goodbyeAgain('world'); |]>goodbyeAgain</button>
>           </%>
>

</div>

<h3><a name="jmacro-antiquotes">Splicing Haskell Values into JavaScript (Antiquotation)</a></h3>

We can also splice Haskell values into the JavaScript code by using <code>&#96;( )&#96;</code>. In the following example, the `onclick` action for the <code>&lt;button&gt;</code> calls `revealFortune()`. The argument to `revealForture` is the `String` returned by evaluating the Haskell expression `fortunes !! n`.

<div class="code">

> fortunePart :: JMacroPart Response
> fortunePart =
>     do let fortunes =
>             ["You will be cursed to write Java for the rest of your days."
>             , "Fortune smiles upon you, your future will be filled with lambdas"
>             ]
>        n <- liftIO $ randomRIO (0, (length fortunes) - 1)
>
>        toResponse <$> defaultTemplate "Fortune"
>               <% [$jmacro|
>                   fun revealFortune fortune
>                   {
>                    var b = document.getElementById("button");
>                    b.setAttribute('disabled', 'disabled');
>                    var p = document.getElementById("fortune");
>                    p.appendChild(document.createTextNode(fortune));
>                   }
>                  |]
>                   %>
>              <div>
>               <h1>Your Fortune</h1>
>               <p id="fortune"></p>
>               <button id="button" onclick=[$jmacro| revealFortune(`(fortunes !! n)`); |]>
>                 Click to reveal your fortune
>               </button>
>              </div>
>

</div>

<h3><a name="jmacro-tojexpr">Using <code>ToJExpr</code> to convert Haskell values to JavaScript</a></h3>

JMacro can embed common types such as `Int`, `Bool`, `Char`, `String`, etc, by default. But we can also embed other types by creating a `ToJExpr` instance for them. For example, let's say we create some types for reporting the weather:

<div class="code">

> data Skies = Cloudy | Clear
>            deriving (Bounded, Enum, Eq, Ord, Read, Show)
>
> newtype Fahrenheit = Fahrenheit Double
>            deriving (Num, Enum, Eq, Ord, Read, Show, ToJExpr, Random)
>
> data Weather = Weather
>     { skies :: Skies
>     , temp  :: Fahrenheit
>     }
>     deriving (Eq, Ord, Read, Show)
>
> instance Random Skies where
>     randomR (lo, hi) g =
>        case randomR (fromEnum lo, fromEnum hi) g of
>          (c, g') -> (toEnum c, g')
>     random g = randomR (minBound, maxBound) g
>
> instance Random Weather where
>     randomR (Weather skiesLo tempLo, Weather skiesHi tempHi) g =
>         let (skies, g') = randomR (skiesLo, skiesHi) g
>             (temp, g'') = randomR (tempLo, tempHi) g'
>         in ((Weather skies temp), g'')
>     random g =
>         let (skies, g') = random g
>             (temp, g'') = random g'
>         in ((Weather skies temp), g'')
>

</div>

To pass these values into the generated JavaScript, we simply create a `ToJExpr` instance:

<div class="code">

#ifdef HsColour
> class ToJExpr a where
>   toJExpr :: a -> JExpr
#endif

</div>

For `Fahrenheit`, we were actually able to derive the `ToJExpr` instance automatically (aka, `deriving (ToJExpr)`), because it is a `newtype` wrapper around `Double` which already has a `ToExpr` instance.

For `Skies`, we can just convert the constructors into JavaScript strings:

<div class="code">

> instance ToJExpr Skies where
>     toJExpr = toJExpr . show
>

</div>

For the `Weather` type, we create a JavaScript object/hash/associative array/record/whatever you want to call it:

<div class="code">

> instance ToJExpr Weather where
>    toJExpr (Weather skies temp) =
>        toJExpr (Map.fromList [ ("skies", toJExpr skies)
>                              , ("temp",  toJExpr temp)
>                              ])
>

</div>
Now we can splice a random weather report into our JavaScript:
<div class="code">

> weatherPart :: JMacroPart Response
> weatherPart =
>     do weather <- liftIO $ randomRIO ((Weather minBound (-40)), (Weather maxBound 100))
>        toResponse <$> defaultTemplate "Weather Report" ()
>         <div>
>          <% [$jmacro|
>              var w = `(weather)`;
>              var p = document.createElement('p');
>              p.appendChild(document.createTextNode("The skies will be " + w.skies +
>                                                    " and the temperature will be " +
>                                                    w.temp.toFixed(1) + "°F"));
>              document.body.appendChild(p);
>              |] %>
>         </div>
>

</div>
`ToJExpr` has an instance for `JSValue` from the <a href="http://hackage.haskell.org/package/json-0.4.4">json library</a>. So, if your type already has a `JSON` istance, you can trivially create a `ToJExpr` instance for it:
<div class="code">

#ifdef HsColour
> instance ToJExpr Foo where
>   toJExpr = toJExpr . showJSON
#endif

</div>

<h3><a name="jmacro-external">Using JMacro in external <kbd>.js</kbd> scripts</a></h3>

So far we have used JMacro to generate JavaScript that is embedded in HTML. We can also use it to create standalone JavaScript.

First we have a script template that is parametrized by a greeting.
<div class="code">

> externalJs :: String -> JStat
> externalJs greeting =
>     [$jmacro|
>      window.greet = function (noun)
>      {
>        alert(`(greeting)` + ' ' + noun);
>      }
>      |]
>

</div>

Notice that we attached the `greet` function to the `window`. The `ToMessage` instance for `JStat` wraps the Javascript in an anonymous function to ensure that statements execute in a local scope. That helps prevents namespace collisions between different external scripts. But, it also means that top-level unhygienic variables will not be global available. So we need to attach them to the `window`.

Next we have a server part with two sub-parts:

<div class="code">

> externalPart :: JMacroPart Response
> externalPart = dir "external" $ msum [

</div>

If <kbd>external/script.js</kbd> is requested, then we check for a query string parameter `greeting` and generate the script. `toResponse` will automatically convert the script to a `Response` and serve it with the content-type, <code>text/javascript; charset=UTF-8</code>:

<div class="code">

>             dir "script.js" $
>                do greeting <- optional $ look "greeting"
>                   ok $ toResponse $ externalJs (fromMaybe "hello" greeting)

</div>

Next we have an html page that includes the external script, and calls the `greet` function:

<div class="code">

>          , toResponse <$> defaultTemplate "external"
>             <script type="text/javascript" src="/external/script.js?greeting=Ahoy" />
>             <div>
>              <h1>Greetings</h1>
>              <button onclick=[$jmacro| greet('JMacro'); |]>Click for a greeting.</button>
>             </div>
>          ]
>

</div>

Instead of attaching the `greet` function to the `window`, we could instead use `jmResponse` to serve the `JStat`. `jmResponse` does not wrap the Javascript in an anonymous function so the `window` work-around is not needed. We do need to use `!` to make sure the name of the `greet2` function is not mangled though:

<div class="code">

> externalJs2 :: String -> JStat
> externalJs2 greeting =
>     [$jmacro|
>      function !greet2 (noun)
>      {
>        alert(`(greeting)` + ' ' + noun);
>      }
>      |]
>
>
> externalPart2 :: JMacroPart Response
> externalPart2 = dir "external2" $ msum
>     [ dir "script.js" $
>           do greeting <- optional $ look "greeting"
>              jmResponse $ externalJs2 (fromMaybe "hello" greeting)
>
>     , toResponse <$> defaultTemplate "external 2"
>        <script type="text/javascript" src="/external2/script.js?greeting=Ahoy" />
>        <div>
>         <h1>Greetings</h1>
>         <button onclick=[$jmacro| greet2('JMacro'); |]>Click for a greeting.</button>
>        </div>
>     ]
>

</div>

<h3><a name="jmacro-demos">Links to demos</a></h3>

Here is a little page that links to all the JMacro demos:

<div class="code">

> demosPart :: JMacroPart Response
> demosPart =
>     toResponse <$> defaultTemplate "demos" ()
>                    <ul>
>                     <li><a href="/hello"    >Hello, JMacro</a></li>
>                     <li><a href="/attr"     >Hello, Attr</a></li>
>                     <li><a href="/endTag"   >Hello, End Tag</a></li>
>                     <li><a href="/clickMe"  >ClickMe</a></li>
>                     <li><a href="/clickMe2" >ClickMe2</a></li>
>                     <li><a href="/functions">Function Names</a></li>
>                     <li><a href="/fortune"  >Fortune</a></li>
>                     <li><a href="/weather"  >Weather</a></li>
>                     <li><a href="/external" >External</a></li>
>                     <li><a href="/external2" >External 2</a></li>
>                    </ul>
>

</div>
and our routes:
<div class="code">

> handlers :: JMacroPart Response
> handlers =
>    msum [ dir "hello"     $ helloJMacro
>         , dir "attr"      $ helloAttr
>         , dir "endTag"    $ helloEndTag
>         , dir "clickMe"   $ clickPart
>         , dir "clickMe2"  $ clickPart2
>         , dir "functions" $ functionNames
>         , dir "fortune"   $ fortunePart
>         , dir "weather"   $ weatherPart
>         , externalPart
>         , externalPart2
>         , demosPart
>         ]
>

</div>

<p class="source-code">[Source code for these demos is <a href="JMacro.hs">here.</a>]</p>

<h3><a name="jmacro-alt-integer-supply">Alternative <code>IntegerSupply</code> instance</a></h3>

If you do not like having to use the `StateT` monad transformer to
generate names, there are other options. For example, we could use
`Data.Unique` to generate unique names:
<div class="code">

#ifdef HsColour
> instance IntegerSupply (ServerPartT IO) where
>     nextInteger = fmap (fromIntegral . (`mod` 1024) . hashUnique) (liftIO newUnique)
#endif

</div>

This should be safe as long as you have less than 1024 different JMacro blocks on a single page.

<h3>More Information</h3>

For more information on using JMacro I recommend reading <a href="http://www.haskell.org/haskellwiki/Jmacro">this wiki page</a> and the tutorial at the top of <a href="http://hackage.haskell.org/packages/archive/jmacro/latest/doc/html/Language-Javascript-JMacro.html">Language.Javascript.JMacro</a>. The documentation is this tutorial has covered the basics of JMacro, but not everything!

