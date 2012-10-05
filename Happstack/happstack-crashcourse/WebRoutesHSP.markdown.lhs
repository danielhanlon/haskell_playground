<!--

> {-# LANGUAGE CPP #-}

-->
<h3><a name="web-route-hsp"><kbd>web-routes</kbd> and <kbd>HSP</kbd></a></h3>

<p class="warning">You will need to install the optional <kbd>web-routes</kbd>, <kbd>web-routes-th</kbd>, <kbd>web-routes-hsp</kbd> and <kbd>happstack-hsp</kbd> packages for this section.</p>

<div class="code">

> {-# LANGUAGE TemplateHaskell #-}
> {-# OPTIONS_GHC -F -pgmFtrhsx #-}
> module Main where
>
> import Control.Applicative ((<$>))
> import Happstack.Server
> import Happstack.Server.HSP.HTML
> import qualified HSX.XMLGenerator as HSX
> import Web.Routes
> import Web.Routes.TH
> import Web.Routes.XMLGenT
> import Web.Routes.Happstack

</div>


If you are using <kbd>web-routes</kbd> and <kbd>HSP</kbd> then inserting URLs is especially clean and easy. If we have the URL:

<div class="code">

> data SiteURL = Monkeys Int deriving (Eq, Ord, Read, Show)
>
> $(derivePathInfo ''SiteURL)
>

</div>

Now we can define a template like this:

<div class="code">

> monkeys :: Int -> RouteT SiteURL (ServerPartT IO) Response
> monkeys n = 
>     do html <- defaultTemplate "monkeys" () $
>         <%>
>          <p>You have <% show n %> monkeys.</p>
>          <p>Click <a href=(Monkeys (succ n))>here</a> for more.</p>
>         </%>
>        ok $ (toResponse html)

</div>

Notice that in particular this bit:

<div class="code">
#ifdef HsColour
> <a href=(Monkeys (succ n))>here</a> 
#endif
</div>

We do not need `showURL`, we just use the URL type directly. That works because `Web.Routes.XMLGenT` provides an instance:


<div class="code">
#ifdef HsColour
> instance (Functor m, Monad m) => EmbedAsAttr (RouteT url m) (Attr String url)
#endif
</div>

Here is the rest of the example:

<div class="code">

> route :: SiteURL -> RouteT SiteURL (ServerPartT IO) Response
> route url =
>     case url of
>       (Monkeys n) -> monkeys n
>
> site :: Site SiteURL (ServerPartT IO Response)
> site = setDefault (Monkeys 0) $ mkSitePI (runRouteT route)
>
> main :: IO ()
> main = simpleHTTP nullConf $
>   msum [ dir "favicon.ico" $ notFound (toResponse ())
>        , implSite (pack "http://localhost:8000") empty site
>        ]

</div>


<p class="source-code">[Source code for the app is <a href="WebRoutesHSP.hs">here.</a>]</p>