<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="rqdatapost">Handling missions</a></h2>

<p>In the previous example we only looked at parameters in the
URL. Looking up values from a form submission (a POST or PUT request)
is almost the same. The only difference is we need to first decode the
request body using <code>decodeBody</code>:

<div class="code">

> {-# LANGUAGE OverloadedStrings #-}
> import Control.Monad                      (msum)
> import Happstack.Server                   ( Response, ServerPart, Method(POST)
>                                           , BodyPolicy(..), decodeBody, defaultBodyPolicy
>                                           , dir, look, nullConf, ok, simpleHTTP
>                                           , toResponse, methodM
>                                           )
> import Text.Blaze                         as B
> import Text.Blaze.Html4.Strict            as B hiding (map)
> import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, label, title)
>
> main :: IO ()
> main = simpleHTTP nullConf $ handlers
>
> myPolicy :: BodyPolicy
> myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)
>
> handlers :: ServerPart Response
> handlers =
>     do decodeBody myPolicy
>        msum [ dir "hello" $ helloPart
>             , helloForm
>             ]
>
> helloForm :: ServerPart Response
> helloForm = ok $ toResponse $
>     html $ do
>       B.head $ do
>         title "Hello Form"
>       B.body $ do
>         form ! enctype "multipart/form-data" ! B.method "POST" ! action "/hello" $ do
>              B.label "greeting: " >> input ! type_ "text" ! name "greeting" ! size "10"
>              B.label "noun: "     >> input ! type_ "text" ! name "noun" ! size "10"
>              input ! type_ "submit" ! name "upload"
>
> helloPart :: ServerPart Response
> helloPart =
>     do methodM POST
>        greeting <- look "greeting"
>        noun     <- look "noun"
>        ok $ toResponse (greeting ++ ", " ++ noun)

</div>
<p class="source-code">[Source code for the app is <a href="RqDataUpload.hs">here.</a>]</p>

<h3><a name="whyDecodeBody">Why is <code>decodeBody</code> even needed?</a></h3>

<p>The body of the HTTP request is ignored unless we call
<code>decodeBody</code>. The obvious question is,
<i>Why isn't the request body automatically decoded?</i>.

<p>If servers had unlimited RAM, disk, CPU and bandwidth available,
then automatically decoding the body would be a great idea. But, since
that is generally not the case, we need a way to limit or ignore form
submission data that is considered excessive.</p>

<p>A simple solution would be to impose a static quota an all form
data submission server-wide. But, in practice, you might want finer
granularity of control. By explicitly calling <code>decodeBody</code>
you can easily configure a site-wide static quota. But you can also
easily adapt the quotas depending on the user, particular form, or
other criteria.</p>

<p>In this example, we keep things simple and just call
<code>decodeBody</code> for all incoming requests. If the incoming
request is not a <code>PUT</code> or <code>POST</code> request with
<code>multipart/form-data</code> then calling <code>decodeBody</code>
has no side-effects.</p>

<h3><a name="bodypolicy">Using <code>BodyPolicy</code> and <code>defaultBodyPolicy</code> to impose quotas</a></h3>

<p>The only argument to <code>decodeBody</code> is a <code>BodyPolicy</code>. The easiest way to define a <code>BodyPolicy</code> is by using the <code>defaultBodyPolicy</code> function:

<div class="code">
#ifdef HsColour
> defaultBodyPolicy :: FilePath  -- ^ directory to *temporarily* store uploaded files in
>                   -> Int64     -- ^ max bytes to save to disk (files)
>                   -> Int64     -- ^ max bytes to hold in RAM (normal form values, etc)
>                   -> Int64     -- ^ max header size (this only affects headers
>                                --                    in the multipart/form-data)
>                   -> BodyPolicy
#endif
</div>

<p>In the example, we define this simple policy:</p>

<div class="code">
#ifdef HsColour
> myPolicy :: BodyPolicy
> myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)
#endif
</div>

<p>Since the form does not do file uploads, we set the file quota to 0. We allow 1000 bytes for the two form fields and 1000 bytes for overhead in the multipart/form-data encoding.</p>

<h3><a name="decodeBody">Using <code>decodeBody</code></a></h3>

<p>Using <code>decodeBody</code> is pretty straight-forward. You simple call it with a <code>BodyPolicy</code>. The key things to know are:

<ol>
 <li>You must call it anytime you are processing a POST or PUT request and you want to use <code>look</code> and friends
 <li><code>decodeBody</code> only works once per request. The first time you call it the body will be decoded. The second time you call it, nothing will happen, even if you call it with a different policy.
</ol>


<h3><a name="otherformtips">Other tips for using <code>&lt;form&gt;</code></a></h3>

<p>When using the <code>&lt;form&gt;</code> element there are two important recommendations you should follow:</p>

<ol>
 <li>Set the <code>enctype</code> to <code>multipart/form-data</code>. This is especially important for forms which contain file uploads.
 <li>Make sure to set <code>method</code> to <code>POST</code> or the form values will show up in the URL as query parameters.
</ol>

