<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="fileservedirectory">Serving Files from a Directory</a></h2>

<p>The most common way to serve files is by using <code>serveDirectory</code>:</p>

<div class="code">
#ifdef HsColour
> data Browsing  = EnableBrowsing | DisableBrowsing
>
> serveDirectory :: ( WebMonad Response m, ServerMonad m, FilterMonad Response m
>                   , MonadIO m, MonadPlus m ) =>
>                   Browsing    -- ^ enable/disable directory browsing
>                -> [FilePath]  -- ^ index file names, used when path is a directory
>                -> FilePath    -- ^ file/directory to serve
>                -> m Response
#endif
</div>

<p>For example:</p>

<div class="code">
#ifdef HsColour
> serveDirectory EnableBrowsing ["index.html"] "path/to/directory/on/disk"
#endif
</div>
<p>If the requested path does not map to a file or directory, then <code>serveDirectory</code> returns <code>mzero</code>.</p>

<p>If the requested path is a file then the file is served normally using <code>serveFile</code>.</p>

<p>When a directory is requested, <code>serveDirectory</code> will first try to find one of the index files (in the order they are listed). If that fails, it will show a directory listing if <code>EnableBrowsing</code>, otherwise it will return <code>forbidden "Directory index forbidden"</code>.</p>

<p>The formula for mapping the URL to a file on disk is just what you would expect:</p>

<div class="code">
path/to/directory/on/disk &lt;/&gt; unconsumed/portion/of/request/url
</div>

<p>So if the handler is:</p>

<div class="code">
#ifdef HsColour
 dir <span style="color: red">"static"</span> $ serveDirectory EnableBrowsing ["index.html"] <span style="color:cyan">"/srv/mysite/datafiles"</span>
#endif
</div>

<p>And the request URL is:</p>

<div class="code">
http://localhost/<span style="color: red">static</span>/<span style="color: green">foo/bar.html</span>
</div>

<p>Then we are going to look for:</p>

<div class="code">
<span style="color:cyan">/srv/mysite/datafiles</span> &lt;/&gt; <span style="color: green">foo/bar.html</span> => <span style="color:cyan">/srv/mysite/datafiles</span>/<span style="color: green">foo/bar.html</span>
</div>

<p>The following demo will allow you to browse the directory that the server is running in. (So be careful where you run it).</p>

<div class="code">

> module Main where
>
> import Happstack.Server (Browsing(EnableBrowsing), nullConf, serveDirectory, simpleHTTP)
>
> main :: IO ()
> main = simpleHTTP nullConf $ serveDirectory EnableBrowsing [] "."

</div>
<p class="source-code">[Source code for the app is <a href="FileServingDirectory.hs">here.</a>]</p>

<p>Simply run it and point your browser at <a href="http://localhost:8000/">http://localhost:8000/</a>.</p>

<h3><a name="fileservesecurity">File Serving Security</a></h3>

<p>The request URL is sanitized so that users can not escape the top-level directory by adding extra <kbd>..</kbd> or <kbd>/</kbd> characters to the URL.</p>

<p>The file serving code <i>will</i> follow symlinks. If you do not want that behavior then you will need to roll your own serving function. See the section on <a href="#fileserveadvanced">Advanced File Serving</a> for more information.</p>