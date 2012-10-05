<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="cookies">Working with Cookies</a></h2>


<h3><a name="what_are_cookies">What are Cookies?</a></h3>
<p>HTTP is a stateless protocol. Each incoming <code>Request</code> is processed
with out any memory of any previous communication with the
client. Though, from using the web, you know that it certainly doesn't
feel that way. A website can remember that you logged in, items in
your shopping cart, etc. That functionality is implemented by using
<code>Cookies</code>.</p>

<p>When the server sends a <code>Response</code> to the client, it can include a special <code>Response</code> header named <code>Set-Cookie</code>, which tells the client to remember a certain <code>Cookie</code>. A <code>Cookie</code> has a name, a string value, and some extra control data, such as a lifetime for the cookie.</p>

<p>The next time the client talks to the server, it will include a copy of the <code>Cookie</code> value in its <code>Request</code> headers. One possible use of cookies is to store a session id. When the client submits the cookie, the server can use the session id to look up information about the client and <i>remember</i> who they are. Sessions and session ids are not built-in to the HTTP specification. They are merely a common idiom which is provided by many web frameworks.</p>

#include "CookieCounter.lhs"
#include "CookieLife.lhs"
#include "CookieIssues.lhs"
#include "CookieFeatures.lhs"