<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="helloheist">Using Heist</a></h2>

<p>Heist is an XML templating engine. The static HTML portions of your web pages reside in XML files which can be edited and reloaded with out having to recompile your server. The dynamic portions are generated in Haskell and spliced into the templates.</p>

<p class="warning">To enable Heist support, you must install the happstack-heist package. It is not installed by default.</p>

<p>The following template is almost an XHTML document, except that it contains the special tag <b>&lt;fact&gt;6&lt;/fact&gt;</b>:

#ifdef HsColour
<div class="code">
<pre>
#include "factorial.tpl-inc"
</pre>
</div>
#endif

<p>The <b>&lt;fact/&gt;</b> tag is an application specific tag which performs a factorial and splices in the result.</p>

<p>The following example shows how to initialize the Heist template system and how to create your own custom tags.</p>

<p>First a bunch of boring imports:</p>

<div class="code">

> module Main where

> import Control.Monad          (msum)
> import Control.Monad.Trans    (MonadIO)
> import qualified Data.Text    as T
> import Happstack.Server       (dir, nullConf, nullDir, simpleHTTP, seeOther, toResponse)
> import Happstack.Server.Heist (templateServe, templateReloader)
> import Text.Templating.Heist  (HeistT, Template, HeistState
>                               , bindSplice, defaultHeistState, getParamNode)
> import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')
> import qualified Text.XmlHtml as X

</div>

<p>Next we have the factorial splice:</p>

<div class="code">

> factSplice :: (Monad m) => HeistT m Template
> factSplice = do
>   input <- getParamNode
>   let text = T.unpack $ X.nodeText input
>       n    = read text :: Int
>   return [X.TextNode $ T.pack $ show $ product [1..n]]
>

</div>
<p>The splice runs in the <code>HeistT</code> monad transformer. The <code>getParamNode</code> function:

<div class="code">
#ifdef HsColour
> getParamNode :: (Monad m) => HeistT m Node
#endif
</div>

<p>returns the XHTML node that triggered this splice function to be called. In this case it would be, <b>&lt;fact&gt;6&lt;/fact&gt;</b></p>

<p>We then use <code>textContent</code>:</p>

<div class="code">
#ifdef HsColour
> X.textContent :: Node -> ByteString
#endif
</div>

<p>to extract the string "6" from the node, which we convert to an Int.</p>

<p>Finally, we calculate the factorial, and convert the result back into the XML that we want to splice into the template.</p>

<p>The mapping from tag names to template functions is stored in the <code>HeistState m</code>. New tags can be added by using <code>bindSplice</code>:</p>

<div class="code">
#ifdef HsColour
> bindSplice :: Monad m => 
>               ByteString               -- ^ name to use for splice tag
>            -> HeistT m Template -- ^ template function to handle the splice
>            -> HeistState m          -- ^ template state to update
>            -> HeistState m
#endif
</div>

<p>So here we bind <b>&lt;fact/&gt;</b> to <code>factSplice</code></p>

<div class="code">

> templateState :: (MonadIO m) => 
>                  FilePath -- ^ path to template directory
>               -> HeistState m
> templateState templateDir = bindSplice (T.pack "fact") factSplice defaultHeistState
>

</div>

<p>In our main function, we must first initialize the Heist template system by using <code>newTemplateDirectory'</code>: </p>

<div class="code">
#ifdef HsColour
> newTemplateDirectory' :: (MonadIO m, MonadIO n) => 
>                          FilePath        -- ^ path to template directory on disk
>                       -> HeistState m -- ^ the template state
>                       -> n (TemplateDirectory m) -- ^ a handle to the template directory
#endif
</div>

<p>In this example, we would put the templates in same directory the app is running from:</p>

<div class="code">

> main :: IO ()
> main = do
>     let templateDir = "."
>     td <- newTemplateDirectory' templateDir (templateState templateDir)

</div>

<p>to serve templates we simply use the <code>templateServe</code> function:</p>

<div class="code">
#ifdef HsColour
> templateServe ::  (ServerMonad m, MonadPlus m, MonadIO m) =>
>                   TemplateDirectory m -- ^ the handle returned by newTemplateDirectory'
>               -> m Response
#endif
</div>

<p><code>templateServe</code> will look at the path in the URL, add .tpl to the end, and try to find a matching template file on disk to return.</p>

<p>Because the templates are loaded into memory, updating the files on disk will not have any immediate effect. You can use <code>templateReloader</code> to force the templates to be reloaded:</p>

<div class="code">
#ifdef HsColour
> templateReloader :: (MonadIO m, MonadIO n) =>
>                     TemplateDirectory m -- ^ handle returned by newTemplateDirectory'
>                  -> n Response
#endif
</div>

<p>so putting those together we get our handlers:</p>

<div class="code">

>     simpleHTTP nullConf $ msum 
>        [ templateServe td
>        , dir "reload" $ nullDir >> templateReloader td
>        , nullDir >> seeOther "/factorial" (toResponse ())
>        ]

</div>
<p class="source-code">[Source code for the app is <a href="TemplatesHeist.hs">here.</a> You will also need to download the source for <a href="factorial.tpl"><kbd>factorial.tpl</kbd></a> and save it in the same directory as  <kbd>TemplateHeist.hs</kbd>.]</p>

<p>If you point your browser at <a href="http://localhost:8000/factorial">http://localhost:8000/factorial</a> you should see the factorial page. You can point your browser at <a href="http://localhost:8000/reload">http://localhost:8000/reload</a> to reload the template.</p>

<h3><a name="moreheist">Other Heist Features</a></h3>

<p>Heist offers a variety of other features not shown here. We have only covered the Happstack integration aspects. For more general information on Heist look at the <a rel="nofollow" href="http://snapframework.com/docs/tutorials/heist">Official Heist Template Tutorial</a> and the <a rel="nofollow" href="http://snapframework.com/docs/latest/heist/index.html">Heist Haddock Documenation</a>.</p>
