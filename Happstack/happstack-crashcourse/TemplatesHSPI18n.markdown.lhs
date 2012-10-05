<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="hsp-i18n">HSP and internationalization (aka, i18n)</a></h2>

<p class="warning">You will need to install happstack-hsp and shakespeare-i18n for this section.</p>

Internationalization (abbreviated to the numeronym i18n) and
localization (L10n) generally refer to the processing of making an
application usuable by people that speak different
languages, use different alphabets and keyboards, and have different
conventions for things like formatting times and dates, currency, etc.

Proper handling of these issues can run deep into your code. For
example, English speakers often think of people as having a first name
and a last name -- but when you look at how people's names are used
around the world, you realize these familiar terms are not universally
applicable. So, a type like:

<div class="code">
#ifdef HsColour
> data Name = Name { firstName :: Text, lastNime :: Text }
>
#endif
</div>

may not be sufficient.

The haskell wiki lists <a href="http://www.haskell.org/haskellwiki/I18N">a bunch of methods</a>
for translating strings into multiple languages.

In this example, we show how we can use native haskell types datas, a
translator friendly file format, and HSP to do some simple
internationalization. We will build on top of the <a href="http://hackage.haskell.org/package/shakespeare-i18n">`shakespeare-i18n`</a> library.

As usual, we start off with a bunch of imports and pragmas:

<div class="code">

> {-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell,
>     MultiParamTypeClasses, OverloadedStrings #-}
> {-# OPTIONS_GHC -F -pgmFtrhsx #-}
> module Main where
>
> import Control.Applicative   ((<$>))
> import Control.Monad         (msum)
> import Control.Monad.Reader  (ReaderT, ask, runReaderT)
> import Control.Monad.Trans   (MonadIO(liftIO))
> import Data.Map              (Map, fromList)
> import qualified Data.Map    as Map
> import           Data.Text   (Text)
> import qualified Data.Text   as Text
> import Happstack.Server      ( ServerPart, ServerPartT, dir, lookTexts', mapServerPartT
>                              , nullConf, nullDir, queryString, simpleHTTP
>                              , acceptLanguage, bestLanguage
>                              )
> import Happstack.Server.HSP.HTML
> import qualified HSX.XMLGenerator as HSX
> import Text.Shakespeare.I18N ( RenderMessage(..), Lang, mkMessage, mkMessageFor
>                              , mkMessageVariant)
> import System.Random (randomRIO)
>

</div>

<h3><a name="hsp-i18n-core-concept">HSP + i18n Core Concept</a></h3>

Instead of using strings directly in our templates we could create a
data type where each constructor represents a phrase, sentence, or
paragraph that we want to put on the page. For example, we could
define the type:

<div class="code">

> data Message = Hello | Goodbye

</div>

Then we could provide a translation function for each language we support:

<div class="code">

> translation_en :: Message -> Text
> translation_en Hello       = Text.pack "hello"
> translation_en Goodbye     = "goodbye"
>
> translation_lojban :: Message -> Text
> translation_lojban Hello   = "coi"
> translation_lojban Goodbye = "co'o"
>
> translations :: Map Text (Message -> Text)
> translations =
>     fromList [ ("en"    , translation_en)
>              , ("lojban", translation_lojban)
>              ]
>
> translate :: Text -> Message -> Text
> translate lang msg =
>     case Map.lookup lang translations of
>       Nothing           -> "missing translation"
>       (Just translator) ->
>           translator msg
>

</div>

and then in our templates we can write:

<div class="code">

> helloPage :: (XMLGenerator m, EmbedAsChild m Text) => Text -> XMLGenT m (HSX.XML m)
> helloPage lang =
>     <html>
>      <head>
>       <title><% translate lang Hello %></title>
>      </head>
>      <body>
>       <p><% translate lang Hello %></p>
>      </body>
>     </html>
>

</div>

The principle behind this approach is nice, but in practice, it has a few problems:

 1. having to write the translation functions in the Haskell source is
    not a very friendly format for the people who will be doing the
    translations.

 2. having to call 'translate' explicitly is boring, tedious, and error prone

 3. having to pass around the desired 'lang' manually is also boring, tedious, and error prone

Fortunately, we can work around all these issues quite simply.

<h3><a name="hsp-i18n-render-message">the <code>RenderMessage</code> class</a></h3>

`shakespeare-i18n` provides a simple class for providing translations:

<div class="code">
#ifdef HsColour
> type Lang = Text
>
> class RenderMessage master message where
>   renderMessage :: master    -- ^ translation variant
>                 -> [Lang]    -- ^ desired languages in descending order of preference
>                 -> message   -- ^ message we want translated
>                 -> Text      -- ^ best matching translation
#endif
</div>

`renderMessage` is pretty straight-forward. It takes a list of preferred languages and a message datatype (such as `Message` type we defined above) and returns the best matching translation. The only mysterious part is the `master` argument. (Personally, I think `variant` might be a better name for the argument). The argument exists so that you can provide more than one set of translations for the same message type.

For example, let's say that we had defined the `Message` type in a library. Being the nice people we are, we also provide a set of translations for the `Message` type. However, someone using our library may want to provide a completely different set of translations that are more appropriate to their application. For example, in the library we might have:

<div class="code">
#ifdef HsColour
> data LibraryI18N = LibraryI18N
>
> instance RenderMessage LibraryI18N Message where
>     renderMessage = ...
#endif
</div>

But the user could provide their own translations for `Message` via:

<div class="code">
#ifdef HsColour
> data AppI18N = AppI18N
>
> instance RenderMessage AppI18N Message where
>     renderMessage = ...
#endif
</div>

<h3><a name="hsp-i18n-shakespeare-i18n"><kbd>shakespeare-i18n</kbd> translation files</a></h3>

Writing the translations in your Haskell source can be pretty inconvenient. Especially if you are working with a team of outsourced translators. Fortunately, `shakespeare-i18n` has support for external translation files.

To keep things simple:

 1. each language will have its own translation file
 2. the file will be named _lang_`.msg` where `lang` is a language code such as `en`, `en-GB`, `fr`, etc
 3. the translation files will all be in a subdirectory which contains nothing but translations
 4. the `.msg` files must be UTF-8 encoded

So for this example we will have three files:

<div class="code">
<pre>
messages/standard/en.msg
messages/standard/en-GB.msg
messages/standard/jbo.msg
</pre>
</div>

 - `en.msg` is a set of generic English translations.
 - `en-GB.msg` is a set of English translations using spellings and idioms common to Great Britain
 - `jbo.msg` is a set of Lojban translations

The contents of the files are:

`messages/standard/en.msg`
<div class="code wide">
#ifdef HsColour
<pre>
#include "messages/standard/en.msg"
</pre>
#endif
</div>

`messages/standard/en-GB.msg`
<div class="code wide">
#ifdef HsColour
<pre>
#include "messages/standard/en-GB.msg"
</pre>
#endif
</div>

`messages/standard/jbo.msg`
<div class="code wide">
#ifdef HsColour
<pre>
#include "messages/standard/jbo.msg"
</pre>
#endif
</div>

The format is very simple. Each line looks like:

<div class="code">
#ifdef HsColour
<pre>
Constructor arg0 arg1 .. argn: translation text
</pre>
#endif
</div>

 1. `Constructor` is a valid Haskell constructor name that we will use to reference this translation
 2. it is followed by 0 or more variable names
 3. then there is a `:`
 4. and then there is the translation

You may also notice that in `en.msg` the arguments contain types like `n@Int`. And some of translations contain markup like `#{show n}`. You can probably guess what those things mean -- we will come back to them shortly.

You may also notice that the Lojban translation is missing the `Problems` constructor. Since there is no translation provided, `renderMessage` will use the default translation (which, in this case will come from `en.msg`).

<!--

Due to TH staging restrictions this code must come before the `mkMessage` call below. But we are not ready to talk about it yet in the tutorial. So it is commented out here.

#ifndef HsColour

> plural_en :: (Integral i) => i -> String -> String -> String
> plural_en 1 x _ = x
> plural_en _ _ y = y
>
> data Thing = TypeError | SegFault deriving (Enum, Bounded, Show)
>
> mkMessageFor "DemoApp" "Thing" "messages/thing" ("en")
>
> thing_tr :: Lang -> Thing -> Text
> thing_tr lang thing = renderMessage DemoApp [lang] thing
>
#endif
-->

To load the message files we first need to define our `master` type:

<div class="code">

> data DemoApp = DemoApp
>

</div>

Then we just call `mkMessage`:

<div class="code">

> mkMessage  "DemoApp" "messages/standard" ("en")
>

</div>

`mkMessage` is a Template Haskell function which:

 1. reads the `.msg` files
 2. creates a new datatype based on the constructors it found
 3. creates a `RenderMessage` instance

`mkMessage` has the following type:

<div class="code">
#ifdef HsColour
> mkMessage :: String    -- ^ name of master translation type
>           -> FilePath  -- ^ path to folder which contains the `.msg` files
>           -> Lang      -- ^ default language
>           -> Q [Dec]
#endif
</div>

If we use `-ddump-splices` we see that the `mkMessages` call above generated the following for us:

<div class="code">
#ifdef HsColour
> data DemoAppMessage
>     = MsgHello
>     | MsgGoodbye
>     | MsgProblems { translationsMessageN     :: Int
>                   , translationsMessageThing :: Thing
>                   }
>
>
> instance RenderMessage DemoApp DemoAppMessage where
>     renderMessage = ...
#endif
</div>

It has created a new type for us `DemoAppMessage` where each constructor is derived from the constructors found in the `en.msg` file. The constructor names all have the prefix `Msg`. That is just to avoid name collisions with the other constructors in your application.

It has also created a `RenderMessage` instance with all the translations (not shown for the sake of readability).

Now we can do:

<div class="code">
<pre>
*Main> renderMessage DemoApp ["en"] MsgHello
"greetings"
</pre>
</div>

Note that because the message files are read in using Template Haskell at compile time, we do not need to install them on the live server. Also, if you change the `.msg` files, you will not see the changes until you recompile.

<h3><a name="hsp-i18n-shakespeare-i18n-variables">Constructor arguments, <code>#{ }</code>, and plurals</a></h3>

The `Problems` constructor in the `en.msg` file appears considerably more complicate than the `Hello` and `Goodbye` cases:

<div class="code wide">
<pre>
Problems n@Int thing@Thing: Got #{show n} #{plural_en n "problem" "problems" } but a #{thing_tr "en" thing} ain't #{plural_en n "it" "one"}.
</pre>
</div>

There are a few things going on here.

<h4><a name="hsp-i18n-shakespeare-i18n-types">Type Annotations</a></h4>

The `Problems` constructor takes two arguments: `n` and `thing`. In order to create the `MsgProblems` constructor, `mkMessage` needs to know the types of those arguments. So, we add the type annotations using the `@` syntax. We only need the type annotations in the default translation file. The default translation file is specified as the third argument to `mkMessage` -- which in this example is `"en"`.

The types of the arguments can be any valid Haskell type. In this case 'Int' and 'Thing'. 'Thing' is just a normal Haskell datatype which we will define right now as:

<div class="code">
#ifdef HsColour
> data Thing = TypeError | SegFault deriving (Enum, Bounded, Show)
#endif
</div>

<h4><a name="hsp-i18n-shakespeare-i18n-splices">Variable Splices</a></h4>

The `#{ }` syntax allows you to call a Haskell function and splice the result into the message. For example:

<div class="code">
#ifdef HsColour
> #{show n}
#endif
</div>

will convert `n` to a `String` and splice the `String` into the message. The expression inside the `#{ }` must be a pure expression and it must have a type that is an instance of the `ToMessage` class:

<div class="code">
#ifdef HsColour
> class ToMessage a where
>   toMessage :: a -> Text
#endif
</div>

By default, only `String` and `Text` have `ToMessage` instances.

Remember that `mkMessage` generates code which gets spliced into the current module. That means the code inside `#{ }` has access to any functions and types which are available in the module that calls `mkMessage`.

<h4><a name="hsp-i18n-shakespeare-i18n-plurals">Handling plurals and other language specifics</a></h4>

In English, we say:

 * I have 1 problem
 * I have 0 problems
 * I have 10 problems

In our translations, we don't want to say *I have 1 problem(s).* We can handle this pluralization issue by creating a simple helper function such as this one:

<div class="code">
#ifdef HsColour
> plural_en :: (Integral i) => i -> String -> String -> String
> plural_en 1 x _ = x
> plural_en _ _ y = y
#endif
</div>

Looking at `en.msg` you notice that we need to use `plural_en` twice to make the grammar sound natural. When creating messages is good to use whole phrases and sentences because changes in one part of a sentence can affect other parts of the sentence. Rules about plurals, word order, gender agreement, etc, vary widely from one language to the next. So it is best to assume as little as possible and give the translators as much flexibility as possible.

<h4><a name="hsp-i18n-shakespeare-i18n-plurals">Translating Existing Types</a></h4>

`mkMessage` creates a new type from the constructors it finds in the `.msg` files. But sometimes we want to create a translation for an existing type. For example, we need to translate the `Thing` type. We can do that by creating a function like:

<div class="code">
#ifdef HsColour
> thing_tr :: Lang -> Thing -> Text
#endif
</div>

Which we can call in the translation file like:

<div class="code">
#ifdef HsColour
> #{thing_tr "en" thing}
#endif
</div>

But, how do we implement `thing_tr`?  One option is to simply write a function like:

<div class="code">
#ifdef HsColour
> thing_tr :: Lang -> Thing -> Text
> thing_tr lang TypeError | lang == "en" = "type error"
> thing_tr lang SegFault  | lang == "en" = "segmentation fault"
> thing_tr _    thing     = thing_tr "en" thing
#endif
</div>

But, now someone has to update the Haskell code to add new translations. It would be nice if all the translations came from `.msg` files.

The `mkMessageFor` function allows us to create translations for an existing type:

<div class="code">
#ifdef HsColour
> mkMessageFor ::
>      String    -- ^ master type
>   -> String    -- ^ data to translate
>   -> FilePath  -- ^ path to `.msg` files
>   -> Lang      -- ^ default language
>   -> Q [Dec]
#endif
</div>

We can create a set of `.msg` files for the `Thing` type like this (note the file path):

`messages/thing/en.msg`
<div class="code">
#ifdef HsColour
<pre>
#include "messages/thing/en.msg"
</pre>
#endif
</div>

And then use `mkMessageFor` to create a `RenderMessage` instance:

<div class="code">
#ifdef HsColour
> mkMessageFor "DemoApp" "Thing" "messages/thing" "en"
#endif
</div>

That will create this instance for us:

<div class="code">
#ifdef HsColour
> -- autogenerated by `mkMessageFor`
> instance RenderMessage DemoApp Thing where
>     renderMessage = ...
#endif
</div>

Because `mkMessageFor` is creating a `RenderMessage` for an existing type, it does not need to append `Message` to the type name or prefix the constructors with `Msg`. Now we can define our `thing_tr` function like this:

<div class="code">
#ifdef HsColour
> thing_tr :: Lang -> Thing -> Text
> thing_tr lang thing = renderMessage DemoApp [lang] thing
#endif
</div>

This is definitely a bit roundabout, but it is the best solution I can see using the existing `shakespeare-i18n` implementation.

<h4><a name="hsp-i18n-shakespeare-alternative">Alternative Translations</a></h4>

We can use `mkMessageVariant` to create an alternative set of
translations for a type that was created by `mkMessage`. For example:

<div class="code">
#ifdef HsColour
> data DemoAppAlt = DemoAppAlt
>
> mkMessageVariant "DemoAppAlt" "DemoApp" "messages/alt" "en"
>
#endif
</div>

<h3><a name="hsp-i18n-embed-as">Using messages in <code>HSX</code> templates</a></h3>

To use the `DemoAppMessage` type in an `HSX` template, all we need is an `EmbedAsChild` instance.

The instance will need to know what the client's preferred languages
are. We can provide that by putting the users language preferences in
a `ReaderT` monad:

<div class="code">

> type I18N  = ServerPartT (ReaderT [Lang] IO)
>

</div>

Next we create the `EmbedAsChild` instance:

<div class="code">

> instance EmbedAsChild I18N DemoAppMessage where
>     asChild msg =
>         do lang <- ask
>            asChild $ renderMessage DemoApp lang msg
>

</div>

Now we can use the message constructors inside our templates:

<div class="code">

> pageTemplate :: (EmbedAsChild I18N body) => String -> body -> I18N XML
> pageTemplate title body =
>     defaultTemplate title ()
>      <div>
>       <% body %>
>       <ul>
>        <% mapM (\lang ->
>                  <li>
>                    <a [ ("href" :: String) := ("?_LANG="++ lang)]><% lang %></a>
>                  </li>)
>               (["en", "en-GB", "jbo"] :: [String]) %>
>       </ul>
>      </div>
>
> homePage :: I18N XML
> homePage =
>    pageTemplate "home"
>        <p><% MsgHello %></p>
>
> goodbyePage :: I18N XML
> goodbyePage =
>     pageTemplate "goodbye"
>         <p><% MsgGoodbye %></p>
>
> problemsPage :: Int -> Thing -> I18N XML
> problemsPage n thing =
>     pageTemplate "problems"
>         <p><% MsgProblems n thing %></p>
>

</div>

Instead of putting text in the `<p> </p>` tags we just use our message constructors.

Getting the language preferences from `ReaderT [Lang]` is just one possibility. Your application may already have a place to store session data that you can get the preferences from, or you might just stick the preferences in a cookie.

<h3><a name="hsp-i18n-detection">Detecting the preferred languages</a></h3>

The `Accept-Language` header is sent by the client and, in theory, specifies what languages the client prefers, and how much they prefer each one. So, in the absence of any additional information, the `Accept-Language` header is a good starting place.  You can retrieve and parse the `Accept-Language` header using the `acceptLanguage` function and then sort the preferences in descending order using `bestLanguage`:

<div class="code">
#ifdef HsColour
> acceptLanguage :: (Happstack m) => m [(Text, Maybe Double)]
> bestLanguage   :: [(Text, Maybe Double)] -> [Text]
#endif
</div>

You should not assume that the `Accept-Language` header is always correct. It is best to allow the user a way to override the `Accept-Language` header. That override could be stored in their user account, session data, a cookie, etc. In this example we will just use a `QUERY_STRING` parameter `_LANG` to override the `Accept-Language` header.

We can wrap this all up in a little function that converts our `I18N` part into a normal `ServerPart`:

<div class="code">

> withI18N :: I18N a -> ServerPart a
> withI18N part =
>     do  langsOverride <- queryString $ lookTexts' "_LANG"
>         langs         <- bestLanguage <$> acceptLanguage
>         mapServerPartT (flip runReaderT (langsOverride ++ langs)) part
>

</div>

And finally, we just have our `route` table and `main` function:

<div class="code">

> routes :: I18N XML
> routes =
>     msum [ do nullDir
>               homePage
>          , dir "goodbye"  $ goodbyePage
>          , dir "problems" $
>              do n     <- liftIO $ randomRIO (1, 99)
>                 let things = [TypeError .. SegFault]
>                 index <- liftIO $ randomRIO (0, length things - 1)
>                 let thing  = things !! index
>                 problemsPage n thing
>          ]
>
>
> main :: IO ()
> main = simpleHTTP nullConf $ withI18N routes

</div>

<p class="source-code">[Source code for the app is <a href="TemplatesHSP.hs">here.</a>]</p>
<p class="source-code">[You will also need to download and unzip the message files <a href="messages.zip">here.</a>]</p>

<h3><a name="hsp-i18n-conclusions">Conclusions</a></h3>

In this section we showed how to use `HSX` and `Happstack.Server.I18N`, and `shakespeare-i18n` together to provide an i18n solution. However, there are no dependencies between those libraries and modules. So, you can use other solutions to provide translations for `HSX`, or you can use `shakespeare-i18n` with other template systems.

One thing that would make `shakespeare-i18n` better is a utility to help keep the `.msg` files up-to-date. I have describe <a href="https://github.com/yesodweb/hamlet/issues/40">my ideas for a tool here</a>. We just need a volunteer to implement it.

