<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="web-routes-boomerang">Web Routes Boomerang</a></h2>

<p>In the previous example we used template haskell to automatically derive a mapping between the url type and the url string. This is very convenient early in the development process when the routes are changing a lot. But the resulting urls are not very attractive. One solution is to write the mappings from the url type to the url string by hand.</p>

<p>One way to do that would be to write one function to show the urls, and another function that uses parsec to parse the urls. But having to say the same thing twice is really annoying and error prone. What we really want is a way to write the mapping once, and automatically exact a parser and printer from the specification.</p>

<p>Fortunately, Sjoerd Visscher and Martijn van Steenbergen figured out exactly how to do that and published a proof of concept library know as <a href="http://hackage.haskell.org/package/Zwaluw"><kbd>Zwaluw</kbd></a>. With permission, I have refactored their original library into two separate libraries: <a href="http://hackage.haskell.org/package/boomerang"><kbd>boomerang</kbd></a> and <a href="http://hackage.haskell.org/package/web-routes-boomerang"><kbd>web-routes-boomerang</kbd></a>.</p>

<p>The technique behind <kbd>Zwaluw</kbd> and <kbd>Boomerang</kbd> is very cool. But in this tutorial we will skip the theory and get right to the practice.</p>

<p>In order to run this demo you will need to install <kbd>web-routes</kbd>, <kbd>web-routes-boomerang</kbd> and <kbd>web-routes-happstack</kbd> from hackage.

<p>We will modify the previous demo to use boomerang in order to demonstrate how easy it is to change methods midstream. We will also add a few new routes to demonstrate some features of using boomerang.</p>

<div class="code">

> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
>   TypeOperators, OverloadedStrings #-}
> module Main where
>

</div>

<p>The first thing to notice is that we hide <code>id</code> and <code>(.)</code> from the <code>Prelude</code> and import the versions from <code>Control.Category</code> instead.</p>

<div class="code">

> import Prelude                 hiding (head, id, (.))
> import Control.Category        (Category(id, (.)))
>
> import Control.Monad           (msum)
> import Data.Data               (Data, Typeable)
> import Data.Monoid             (mconcat)
> import Data.String             (fromString)
> import Data.Text               (Text)
> import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
>                                , nullConf, seeOther, dir, notFound, seeOther)
> import Text.Blaze.Html4.Strict ( (!), html, head, body, title, p, toHtml
>                                , toValue, ol, li, a)
> import Text.Blaze.Html4.Strict.Attributes (href)
> import Text.Boomerang.TH       (derivePrinterParsers)
> import Web.Routes              ( PathInfo(..), RouteT, showURL
>                                , runRouteT, Site(..), setDefault, mkSitePI)
> import Web.Routes.TH           (derivePathInfo)
> import Web.Routes.Happstack    (implSite)
> import Web.Routes.Boomerang
>

</div>

<p>Next we have our <code>Sitemap</code> types again. <code>Sitemap</code> is similar to the previous example, except it also includes <code>UserOverview</code> and <code>UserDetail</code>.</p>

<div class="code">

> newtype ArticleId
>     = ArticleId { unArticleId :: Int }
>       deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
>
> data Sitemap
>     = Home
>     | Article ArticleId
>     | UserOverview
>     | UserDetail Int Text
>       deriving (Eq, Ord, Read, Show, Data, Typeable)
>

</div>

<p>Next we call <code>derivePrinterParsers</code>:</p>

<div class="code">

> $(derivePrinterParsers ''Sitemap)
>

</div>

<p>That will create new combinators corresponding to the constructors
for <code>Sitemap</code>. They will be named, <code>rHome</code>, <code>rArticle</code>, <code>rUserOverview</code>, and <code>rUserDetail</code>.</p>

<p>Now we can specify how the <code>Sitemap</code> type is mapped to a url string and back:</p>

<div class="code">

> sitemap :: Router () (Sitemap :- ())
> sitemap =
>     (  rHome
>     <> rArticle . (lit "article" </> articleId)
>     <> lit "users" . users
>     )
>     where
>       users =  rUserOverview
>             <> rUserDetail </> int . lit "-" . anyText
>
> articleId :: Router () (ArticleId :- ())
> articleId =
>     xmaph ArticleId (Just . unArticleId) int

</div>

<p>The mapping looks like this:</p>

<div class="code">
<table>
 <thead>
  <tr><th>url</th><th></th><th>type</th></tr>
 </thead>
 <tbody>
  <tr><td>/</td><td>&lt;=&gt;</td><td>Home</td></tr>
  <tr><td>/article/<i>int</i></td><td>&lt;=&gt;</td><td>Article <i>int</i></td></tr>
  <tr><td>/users</td><td>&lt;=&gt;</td><td>UserOverview</td></tr>
  <tr><td>/users/<i>int</i>-<i>string</i></td><td>&lt;=&gt;</td><td>UserDetail <i>int</i> <i>string</i></td></tr>
 </tbody>
</table>
</div>

<p>The <code>sitemap</code> function looks like an ordinary parser. But, what makes it is exciting is that it also defines the pretty-printer at the same time.</p>

<p>By examining the mapping table and comparing it to the code, you should be able to get an intuitive feel for how <kbd>boomerang</kbd> works. The key boomerang features we see are:</p>
<dl>
 <dt><code>&lt;&gt;</code></dt>
 <dd><code>&lt;&gt;</code> is the choice operator. It chooses between the various paths.</dd>
 <dt><code>.</code></dt>
 <dd><code>.</code> is used to combine elements together.</dd>
 <dt><code>&lt;/&gt;</code></dt>
 <dd>the combinators, such as <code>lit</code>, <code>int</code>, <code>anyText</code>, operate on a single path segment. <code>&lt;/&gt;</code> matches on the / between path segments.</dd>
 <dt><code>lit</code></dt>
 <dd><code>lit</code> matches on a string literal. If you enabled <code>OverloadedStrings</code> then you do not need to explicitly use the <code>lit</code> function. For example, you could just write, <code>int . "-" . anyText</code>.</dd>
 <dt><code>int</code></dt>
 <dd><code>int</code> matches on an <code>Int</code>.</dd>
 <dt><code>anyText</code></dt>
 <dd><code>anyText</code> matches on any string. It keeps going until it reaches the end of the current path segment.</dd>
 <dt><code>xmaph</code></dt>
 <dd><code>xmaph</code> is a bit like <code>fmap</code>, except instead of only needing <code>a -> b</code> it also needs the other direction, <code>b -> Maybe a</code>.
<div class="code">
#ifdef HsColour
> xmaph :: (a -> b)
>       -> (b -> Maybe a)
>       -> PrinterParser e tok i (a :- o)
>       -> PrinterParser e tok i (b :- o)
#endif
     </div>
   In this example, we use <code>xmaph</code> to convert <code>int :: Router () (Int :- ())</code> into <code>articleId :: Router () (ArticleId :- ())</code>.
 </dd>
 <dt>longest route</dt>
 <dd>You will notice that the parser for <code>/users</code> comes before <code>/users/<i>int</i>-<i>string</i></code>. Unlike <kbd>parsec</kbd>, the order of the parsers (usually) does not matter. We also do not have to use <kbd>try</kbd> to allow for backtracking. <kbd>boomerang</kbd> will find all valid parses and pick the best one. Here, that means the parser that consumed all the available input.</dd>
</dl>

<p><code>Router</code> type is just a simple alias:</p>

<div class="code">
#ifdef HsColour
> type Router a b = PrinterParser TextsError [Text] a b
#endif
</div>

<p>Looking at this line:</p>

<div class="code">
#ifdef HsColour
>             <> rUserDetail </> int . lit "-" . anyText
#endif
</div>

<p>and comparing it to the constructor</p>

<div class="code">
#ifdef HsColour
>     UserDetail Int Text
#endif
</div>

<p>we see that the constructor takes two arguments, but the mapping uses three combinators, <code>int</code>, <code>lit</code>, and <code>anyText</code>. It turns out that some combinators produce/consume values from the url type, and some do not. We can find out which do and which don't by looking at the their types:</p>

<div class="code">
#ifdef HsColour
> int     ::         PrinterParser TextsError [Text] r (Int :- r)
> anyText ::         PrinterParser TextsError [Text] r (Text :- r)
> lit     :: Text -> PrinterParser TextsError [Text] r r
#endif
</div>

<p>We see <code>int</code> takes <code>r</code> and produces <code>(Int :- r)</code> and <code>anyText</code> takes <code>r</code> and produces <code>(Text :- r)</code>. While <code>lit</code> takes <code>r</code> and returns <code>r</code>.</p>

<p>Looking at the type of the all three composed together we get:</p>

<div class="code">
#ifdef HsColour
> int . lit "-" . anyText :: PrinterParser TextsError [Text] a (Int :- (Text :- a))
#endif
</div>

<p>So there we see the <code>Int</code> and <code>Text</code> that are arguments to <code>UserDetail</code>.</p>

<p>Looking at the type of <code>rUserDetail</code>, we will see that it has the type:</p>

<div class="code">
#ifdef HsColour
>  rUserDetail :: PrinterParser e tok (Int :- (Text :- r)) (Sitemap :- r)
#endif
</div>

<p>So, it takes an <code>Int</code> and <code>Text</code> and produces a <code>Sitemap</code>. That mirrors what the <code>UserDetail</code> constructor itself does:</p>

<div class="code">
<pre>
ghci> :t UserDetail
UserDetail :: Int -> Text -> Sitemap
</pre>
</div>

<p>Next we need a function that maps a route to the handlers. This is the same exact function we used in the previous example extended with the additional routes:</p>

<div class="code">

> route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
> route url =
>     case url of
>       Home                  -> homePage
>       (Article articleId)   -> articlePage articleId
>       UserOverview          -> userOverviewPage
>       (UserDetail uid name) -> userDetailPage uid name
>

</div>

<p>Next, we have the handler functions. These are also exactly the same as the previous example, plus the new routes:</p>

<div class="code">

> homePage :: RouteT Sitemap (ServerPartT IO) Response
> homePage =
>     do articles     <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
>        userOverview <- showURL UserOverview
>        ok $ toResponse $
>           html $ do
>             head $ title $ "Welcome Home!"
>             body $ do
>               a ! href (toValue userOverview) $ "User Overview"
>               ol $ mconcat articles
>     where
>       mkArticle articleId =
>           do url <- showURL (Article articleId)
>              return $ li $ a ! href (toValue url) $
>                         toHtml $ "Article " ++ (show $ unArticleId articleId)
>

</div>
<div class="code">

> articlePage :: ArticleId -> RouteT Sitemap (ServerPartT IO) Response
> articlePage (ArticleId articleId) =
>     do homeURL <- showURL Home
>        ok $ toResponse $
>           html $ do
>             head $ title $ (toHtml $ "Article " ++ show articleId)
>             body $ do
>                    p $ toHtml $ "You are now reading article " ++ show articleId
>                    p $ do "Click "
>                           a ! href (toValue homeURL) $ "here"
>                           " to return home."
>

</div>

<div class="code">

> userOverviewPage :: RouteT Sitemap (ServerPartT IO) Response
> userOverviewPage =
>     do users <- mapM mkUser [1 .. 10]
>        ok $ toResponse $
>           html $ do
>             head $ title $ "Our Users"
>             body $ do
>               ol $ mconcat users
>     where
>       mkUser userId =
>           do url <- showURL (UserDetail userId (fromString $ "user " ++ show userId))
>              return $ li $ a ! href (toValue url) $
>                         toHtml $ "User " ++ (show $ userId)
>

</div>

<div class="code">

> userDetailPage :: Int -> Text -> RouteT Sitemap (ServerPartT IO) Response
> userDetailPage userId userName =
>     do homeURL <- showURL Home
>        ok $ toResponse $
>           html $ do
>             head $ title $ (toHtml $ "User " <> userName)
>             body $ do
>                    p $ toHtml $ "You are now view user detail page for " <> userName
>                    p $ do "Click "
>                           a ! href (toValue homeURL) $ "here"
>                           " to return home."
>

</div>


<p>Creating the <code>Site</code> type is similar to the previous example. We still use <code>runRouteT</code> to unwrap the <code>RouteT</code> layer. But now we use <code>boomerangSite</code> to convert the <code>route</code> function into a <code>Site</code>: </p>

<div class="code">

> site :: Site Sitemap (ServerPartT IO Response)
> site =
>        setDefault Home $ boomerangSite (runRouteT route) sitemap
>

</div>

<p>The route function is essentially the same in this example and the previous example -- it did not have to be changed to work with <code>boomerang</code> instead of <code>PathInfo</code>. It is the <code>formatPathSegments</code> and <code>parsePathSegments</code> functions bundled up in the <code>Site</code> that change. In the previous example, we used <code>mkSitePI</code>, which leveraged the <code>PathInfo</code> instances. Here we use <code>boomerangSite</code> which uses the <code>sitemap</code> mapping we defined above.</p>

<p>The practical result is that you can start by using <code>derivePathInfo</code> and avoid having to think about how the urls will look. Later, once the routes have settled down, you can then easily switch to using <code>boomerang</code> to create your route mapping.</p>

<p>Next we use <code>implSite</code> to embed the <code>Site</code> into a normal Happstack route:</p>

<div class="code">

> main :: IO ()
> main = simpleHTTP nullConf $
>        msum [ dir "favicon.ico" $ notFound (toResponse ())
>             , implSite "http://localhost:8000" "/route" site
>             , seeOther ("/route/" :: String) (toResponse ())
>             ]
>

</div>
<p class="source-code">[Source code for the app is <a href="WebRoutesBoomerang.hs">here.</a>]</p>

<p>In this example, we only used a few simple combinators. But <kbd>boomerang</kbd> provides a whole range of combinators such as many, some, chain, etc. For more information check out the <a href="http://hackage.haskell.org/package/boomerang">haddock documentation</a> for <kbd>boomerang</kbd>. Especially the <code>Text.Boomerang.Combinators</code> and <code>Text.Boomerang.Texts</code> modules.</p>
