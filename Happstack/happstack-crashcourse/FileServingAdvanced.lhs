<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="fileserveadvanced">Advanced File Serving</a></h2>

<p><code>serveDirectory</code> and <code>serveFile</code> should cover a majority of your file serving needs. But if you want something a little different, it is also possible to roll-your-own solution. The <code>Happstack.Server.FileServe.BuildingBlocks</code> module contains all the pieces used to assemble the high-level <code>serveDirectory</code> and <code>serveFile</code> functions. You can reuse those pieces to build your own custom serving functions. For example, you might want to use a different method for calculating the mime-types, or perhaps you want to create a different look-and-feel for directory browsing, or maybe you want to use something other than <code>sendFile</code> for sending the files. I recommend starting by copying the source for <code>serveDirectory</code> or <code>serveFile</code> and then modifying it to suit your needs.</p>
