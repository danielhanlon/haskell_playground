<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="rqdataupload">File Uploads</a></h2>

<p>The <code>lookFile</code> function is used to extract an uploaded file:</p>

<div class="code">
#ifdef HsColour
> lookFile :: String -> RqData (FilePath, FilePath, ContentType)
#endif
</div>

<p>It returns three values:</p>
<ol>
 <li>The location of the temporary file which holds the contents of the file</li>
 <li>The <i>local</i> filename supplied by the browser. This is typically the name of the file on the users system.</li>
 <li>The content-type of the file (as supplied by the browser)</li>
</ol>

<p>The temporary file will be automatically deleted after the <code>Response</code> is sent. Therefore, it is essential that you move the file from the temporary location.</p>

<p>In order for file uploads to work correctly, it is also essential that your &lt;form&gt; element contains the attributes <code>enctype="multipart/form-data"</code> and <code>method="POST"</code></p>

<p>The following example has a form which allows a user to upload a file. We then show the temporary file name, the uploaded file name, and the content-type of the  file. In a real application, the code should use <code>System.Directory.renameFile</code> (or similar) to move the temporary file to a permanent location. This example looks a bit long, but most of the code is just HTML generation using BlazeHtml. The only really new part is the use of the <code>lookFile</code> function. Everything else should already have been covered in previous sections. So if you don't understand something, try looking in earlier material.</p>

<div class="code">

> {-# LANGUAGE OverloadedStrings #-}
> import Control.Monad                      (msum)
> import Happstack.Server                   ( Response, ServerPart, defaultBodyPolicy
>                                           , decodeBody, dir, lookFile, nullConf, ok
>                                           , simpleHTTP, toResponse )
> import Text.Blaze                         as B
> import Text.Blaze.Html4.Strict            as B hiding (map)
> import Text.Blaze.Html4.Strict.Attributes as B hiding (dir, title)
>
> main :: IO ()
> main = simpleHTTP nullConf $ upload
>
> upload :: ServerPart Response
> upload =
>     do decodeBody (defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000)
>        msum [ dir "post" $ post
>             , uploadForm
>             ]
>
> uploadForm :: ServerPart Response
> uploadForm = ok $ toResponse $
>     html $ do
>       B.head $ do
>         title "Upload Form"
>       B.body $ do
>         form ! enctype "multipart/form-data" ! B.method "POST" ! action "/post" $ do
>              input ! type_ "file" ! name "file_upload" ! size "40"
>              input ! type_ "submit" ! value "upload"
>
> post :: ServerPart Response
> post =
>    do r <- lookFile "file_upload"
>       ok $ toResponse $
>          html $ do
>            B.head $ do
>              title "Post Data"
>            B.body $ mkBody r
>     where
>       mkBody (tmpFile, uploadName, contentType) = do
>                 p (toHtml $ "temporary file: " ++ tmpFile)
>                 p (toHtml $ "uploaded name:  " ++ uploadName)
>                 p (toHtml $ "content-type:   " ++ show contentType)

</div>
<p class="source-code">[Source code for the app is <a href="RqDataUpload.hs">here.</a>]</p>

<h3><a name="file_upload_reminder">File uploads important reminder</a></h3>

<p>Remember that you must move the temporary file to a new location or it will be garbage collected after the Response is sent. In the example code we do <b>not</b> move the file, so it is automatically deleted.</p>