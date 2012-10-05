<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="web-routes-demo">Web Routes Demo</a></h2>

<p>Let's start by looking at a simple example of using <code>web-routes</code>. In this example we will use blaze for the html templates.</p>

<p>In order to run this demo you will need to install <kbd>web-routes</kbd>, <kbd>web-routes-th</kbd> and <kbd>web-routes-happstack</kbd> from hackage.

<div class="code">

> {-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
> module Main where
>
> import Prelude                 hiding (head)
>
> import Control.Monad           (msum)
> import Data.Data               (Data, Typeable)
> import Data.Monoid             (mconcat)
> import Data.Text               (pack)
> import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
>                                , nullConf, seeOther, dir, notFound, seeOther)
> import Text.Blaze.Html4.Strict ( (!), html, head, body, title, p, toHtml
>                                , toValue, ol, li, a)
> import Text.Blaze.Html4.Strict.Attributes (href)
> import Web.Routes              ( PathInfo(..), RouteT, showURL
>                                , runRouteT, Site(..), setDefault, mkSitePI)
> import Web.Routes.TH           (derivePathInfo)
> import Web.Routes.Happstack    (implSite)
>

</div>
<p>First we need to define the type to represent our routes. In this site we will have a homepage and articles which can be retrieved by their id.</p>
<div class="code">

> newtype ArticleId
>     = ArticleId { unArticleId :: Int }
>       deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
>
> data Sitemap
>     = Home
>     | Article ArticleId
>       deriving (Eq, Ord, Read, Show, Data, Typeable)
>

</div>

<p>Next we use template-haskell to derive an instance of <code>PathInfo</code> for the <code>Sitemap</code> type.</p>

<div class="code">

> $(derivePathInfo ''Sitemap)
>

</div>

<p>The <code>PathInfo</code> class is defined in <code>Web.Routes</code> and looks like this:</p>

<div class="code">
#ifdef HsColour
> class PathInfo a where
>   toPathSegments :: a -> [String]
>   fromPathSegments :: URLParser a
#endif
</div>

<p>It is basically a class that describes how to turn a type into a
url and back. This class is semi-optional. Some conversion methods
such as <kbd>web-routes-th</kbd> and <kbd>web-routes-regular</kbd> use
it, but others do not.</p>

<p>Since <code>ArticleId</code> is just a <code>newtype</code> we were
able to just do <code>deriving PathInfo</code> instead of having
to call <code>derivePathInfo</code>.

<p>Next we need a function that maps a route to the handlers:</p>

<div class="code">

> route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
> route url =
>     case url of
>       Home                -> homePage
>       (Article articleId) -> articlePage articleId
>

</div>

<p>As you can see, mapping a url to a handler is just a straight-forward case statement. We do not need to do anything fancy here to extract the article id from the url, becuse that has already been done when the url was converted to a <code>Sitemap</code> value.</p>

<p>You may be tempted to write the <code>route</code> function like this instead of using the case statement:</p>

<div class="code">
#ifdef HsColour
> route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
> route Home                = homePage
> route (Article articleId) = articlePage articleId
#endif
</div>

<p>But, I don't recommend it. In a real application, the <code>route</code> function will likely take a number of extra arguments such as database handles. Sometimes those extra arguments are only used by some of the handlers. But every time you add a parameter, you have to update every pattern match to account for the extra argument. Using a case statement instead makes the code easier to maintain and more readable in my opinion.</p>

<p>The other thing you will notice is the <code>RouteT</code> monad transformer in the type signature. The <code>RouteT</code> monad transformer is another semi-optional feature of <kbd>web-routes</kbd>. <code>RouteT</code> is basically a <code>Reader</code> monad that holds the function which converts the url type into a string. At first, this seems unnecessary -- why not just call <code>toPathInfo</code> directly and skip <code>RouteT</code> entirely? But it turns out there are few advantages that <code>RouteT</code> brings:</p>

<ol>
 <li><code>RouteT</code> is parametrized by the url type -- in this case <code>Sitemap</code>. That will prevent us from accidentally trying to convert an <code>ArticleId</code> into a url. An <code>ArticleId</code> is a valid component of some urls, but it is not a valid URL by itself.</li>
 <li>The url showing function inside <code>RouteT</code> can also contain additional information needed to form a valid url, such as the hostname name, port, and path prefix</li>
 <li><code>RouteT</code> is also used when we want to embed a library/sub-site into a larger site.</li>
</ol>

<p>We will see examples of these benefits as we continue with the tutorial.</p>

<p>Next, we have the handler functions:</p>

<div class="code">

> homePage :: RouteT Sitemap (ServerPartT IO) Response
> homePage =
>     do articles <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
>        ok $ toResponse $
>           html $ do
>             head $ title $ (toHtml "Welcome Home!")
>             body $ do
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
>                    p $ do toHtml "Click "
>                           a ! href (toValue homeURL) $ toHtml "here"
>                           toHtml " to return home."
>

</div>

<p>Even though we have the <code>RouteT</code> in the type signature -- these functions look normal <code>ServerPartT</code> functions -- we do not have to use <code>lift</code> or anything else. That is because <code>RouteT</code> is a instance of all the Happstack classes such as <code>ServerMonad,</code> <code>FilterMonad,</code> etc. Though you do need to make sure you have imported <code>Web.Routes.Happstack</code> to get those instances.</p>

<p>The only new thing here is the <code>showURL</code> function, which has the type:</p>

<div class="code">
#ifdef HsColour
> showURL :: ShowURL m => URL m -> m String
#endif
</div>

<p><code>showURL</code> converts a url type, such as <code>Sitemap</code> into a url <code>String</code> that we can use an <kbd>href</kbd>, <kbd>src</kbd>, etc attribute.</p>

<p><code>URL m</code> is a type-function that calculates the url type based on the monad we are currently in. For <code>RouteT url m a</code>, <code>URL m</code> is going to be whatever <code>url</code> is. In this example, <code>url</code> is <code>Sitemap</code>. If you are not familiar with type families and type functions, see <a href="#web-routes-type-families">this section</a>.</p>

<p>Now we have:</p>
<ol>
 <li>A url type, <code>Sitemap</code></li>
 <li>functions to convert the type to a string and back via <code>PathInfo</code></li>
 <li>a function to route the url to a handler, <code>route</code></li>
</ol>

<p>We need to tie these three pieces together. That is what the <code>Site</code> type does for us:</p>

<div class="code">
#ifdef HsColour
> data Site url a
>     = Site {
>            -- | function which routes the url to a handler
>              handleSite         :: (url -> [(String, String)] -> String) -> url -> a
>            -- | This function must be the inverse of 'parsePathSegments'.
>            , formatPathSegments :: url -> ([String], [(String, String)])
>            -- | This function must be the inverse of 'formatPathSegments'.
>            , parsePathSegments  :: [String] -> Either String url
>            }
#endif
</div>

<p>Looking at the type for <code>Site</code>, we notice that it is very general -- it does not have any references to <kbd>Happstack</kbd>, <code>PathInfo</code>, <code>URLParser</code>, <code>RouteT</code>, etc. That is because those are all addons to the core of <kbd>web-routes</kbd>. We can convert our <code>route</code> to a <code>Site</code> using some simple helper functions like this: </p>

<div class="code">

> site :: Site Sitemap (ServerPartT IO Response)
> site =
>        setDefault Home $ mkSitePI (runRouteT route)
>

</div>

<p><code>runRouteT</code> removes the <code>RouteT</code> wrapper from our routing function:</p>

<div class="code">
#ifdef HsColour
> runRouteT :: (url -> RouteT url m a)
>           -> ((url -> [(String, String)] -> String) -> url -> m a)
#endif
</div>

<p>So if we have our routing function like:</p>

<div class="code">
#ifdef HsColour
> route :: Sitemap
>       -> RouteT Sitemap (ServerPartT IO) Response
#endif
</div>

<p><code>runRouteT</code> will convert it to a function that takes a url showing function:</p>

<div class="code">
#ifdef HsColour
> (runRouteT route) :: (Sitemap -> [(String, String)] -> String)
>                   -> Sitemap
>                   -> ServerPartT IO Response
#endif
</div>

<p>Since we created a <code>PathInfo</code> instance for
<code>Sitemap</code> we can use <code>mkSitePI</code> to convert the
new function to a <code>Site</code>. <code>mkSitePI</code> has the type:</p>

<div class="code">
#ifdef HsColour
> mkSitePI :: (PathInfo url) =>
>             ((url -> [(String, String)] -> String) -> url -> a)
>          -> Site url a
#endif
</div>

<p>so applying it to <code>runRouteT route</code> gives us:</p>

<div class="code">
#ifdef HsColour
> (mkSitePI (runRouteT route)) :: Site Sitemap (ServerPartT IO Response)
#endif
</div>

<p><code>setDefault</code> allows you to map <kbd>/</kbd> to any route you want. In this example we map <kbd>/</kbd> to <code>Home</code>.</p>

<div class="code">
#ifdef HsColour
> setDefault :: url -> Site url a -> Site url a
#endif
</div>

<p>Next we use <code>implSite</code> to embed the <code>Site</code> into a normal Happstack route:</p>

<div class="code">

> main :: IO ()
> main = simpleHTTP nullConf $
>        msum [ dir "favicon.ico" $ notFound (toResponse ())
>             , implSite (pack "http://localhost:8000") (pack "/route") site
>             , seeOther "/route" (toResponse ())
>             ]
>

</div>

<p>The type for implSite is straight-forward:</p>

<div class="code">
#ifdef HsColour
> implSite :: (Functor m, Monad m, MonadPlus m, ServerMonad m) =>
>             String         -- ^ "http://example.org"
>          -> FilePath       -- ^ path to this handler, .e.g. "/route"
>          -> Site url (m a) -- ^ the 'Site'
>          -> m a
#endif
</div>

<p>The first argument is the domain/port/etc that you want to add to the beginning of any URLs you show. The first argument is not used during the decoding/routing process -- it is merely prepended to any generated url strings.</p>

<p>The second argument is the path to this handler. This path is automatically used when routing the incoming request and when showing the URL. This path can be used to ensure that all routes generated by <kbd>web-routes</kbd> are unique because they will be in a separate sub-directory (aka, a separate namespace). If you do not want to put the routes in a separate sub-directory you can set this field to "".</p>

<p>The third argument is the <code>Site</code> that does the routing.</p>

<p>If the URL decoding fails, then <code>implSite</code> will call <code>mzero</code>.</p>

<p>Sometimes you will want to know the exact parse error that caused the router to fail. You can get the error by using <code>implSite_</code> instead. Here is an alternative <code>main</code> that prints the route error to <kbd>stdout</kbd>.</p>

<div class="code">
#ifdef HsColour
> main :: IO ()
> main = simpleHTTP nullConf $
>        msum [ dir "favicon.ico" $ notFound (toResponse ())
>             , do r <- implSite_ (pack "http://localhost:8000") (pack "/route") site
>                  case r of
>                    (Left e) -> liftIO (print e) >> mzero
>                    (Right m) -> return m
>             , seeOther "/route" (toResponse ())
>             ]
>
#endif
</div>

<p class="source-code">[Source code for the app is <a href="WebRoutesDemo.hs">here.</a>]</p>

<h3><a name="web-routes-type-families">Web Routes + Type Families</a></h3>

<p><code>showURL</code> has the type:</p>

<div class="code">
#ifdef HsColour
> showURL :: ShowURL m => URL m -> m String
#endif
</div>

<p>If you are not familiar with type families and type functions, the <code>URL m</code> in that type signature might look a bit funny. But it is really very simple.</p>

<p>The <code>showURL</code> function leverages the <code>ShowURL</code> class:</p>

<div class="code">
#ifdef HsColour
> class ShowURL m where
>    type URL m
>    showURLParams :: (URL m) -> [(String, String)] -> m String
#endif
</div>

<p>And here is the <code>RouteT</code> instance for <code>ShowURL</code>:</p>

<div class="code">
#ifdef HsColour
> instance (Monad m) => ShowURL (RouteT url m) where
>    type URL (RouteT url m) = url
>    showURLParams url params =
>        do showF <- askRouteT
>           return (showF url params)
#endif
</div>

<p>Here <code>URL</code> is a <i>type function</i> that is applied to a type and gives us another type. For example, writing <code>URL (RouteT Sitemap (ServerPartT IO))</code> gives us the type <code>Sitemap</code>. We can use the type function any place we would normally use a type.</p>

<p>In our example we had:</p>

<div class="code">
#ifdef HsColour
> homeURL <- showURL Home
#endif
</div>

<p>So there, showURL is going to have the type:</p>

<div class="code">
#ifdef HsColour
> showURL :: URL (RouteT Sitemap (ServerPartT IO))
>         -> RouteT Sitemap (ServerPartT IO) String
#endif
</div>

<p>which can be simplified to:</p>

<div class="code">
#ifdef HsColour
> showURL :: Sitemap -> RouteT Sitemap (ServerPartT IO) String
#endif
</div>

<p>So, we see that the url type we pass to <code>showURL</code> is dictated by the monad we are currently in. This ensures that we only call <code>showURL</code> on values of the right type.</p>

<p>While <code>ShowURL</code> is generally used with the <code>RouteT</code> type -- it is not actually a requirement. You can implement <code>ShowURL</code> for any monad of your choosing.</p>

