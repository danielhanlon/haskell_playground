<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="cookie_issues">Cookie Issues</a></h3>

<p>Despite their apparently simplicity, <code>Cookies</code> are the source of many bugs and security issues in web applications. Here are just a few of the things you need to keep in mind.</p>

<h4><a name="cookie_security">Security issues</a></h4>

<p>To get an understanding of cookie security issues you should search for <a href="http://www.google.com/search?q=cookie+security+issues">cookie security issues</a> and <a href="http://www.google.com/search?q=cookie+XSS">cookie XSS</a></p>

<p>One important thing to remember is that the user can modify the cookie. So it would be a bad idea to do, <code>addCookie Session (mkCookie "userId" "1234")</code> because the user could modify the cookie and change the userId at will to access other people's accounts.</p>

<p>Also, if you are not using <code>https</code> the cookie will be sent unencrypted.</p>

<h4><a name="cookie_delayed">Delayed Effect</a></h4>

<p>When you call <code>addCookie</code> the <code>Cookie</code> will not be available until after that <code>Response</code> has been sent and a new <code>Request</code> has been received. So the following code will not work:</p>

<div class="code">
#ifdef HsColour
> do addCookie Session (mkCookie "newCookie" "newCookieValue")
>    v <- look "newCookie"
>    ...
> 
#endif 
</div>

<p>The first time it runs, <code>look</code> will fail because the cookie was not set in the current <code>Request</code>. Subsequent times <code>look</code> will return the old cookie value, not the new value.</p>

<h4><a name="cookie_size">Cookie Size</a></h4>

<p>Browsers impose limits on how many cookies each site can issue, and how big those cookies can be. The RFC recommends browsers accept a minimum of 20 cookies per site, and that cookies can be at least 4096 bytes in size. But, implementations may vary. Additionally, the cookies will be sent with every request to the domain. If your page has dozens of images, the cookies will be sent with every request. That can add a lot of overhead and slow down site loading times.</p>

<p>A common alternative is to store a small session id in the cookie, and store the remaining information on the server, indexed by the session id. Though that brings about its own set of issues.</p>

<p>One way to avoid having cookies sent with every image request is to host the images on a different sub-domain. You might issues the cookies to www.example.org, but host images from images.example.org. Note that you do not actually have to run two servers in order to do that. Both domains can point to the same IP address and be handled by the same application. The app itself may not even distinguish if the requests were sent to <code>images</code> or <code>www</code>.</p>

<h4><a name="cookie_clock">Server Clock Time</a></h4>

<p>In order to calculate the <code>expires</code> date from the <code>max-age</code> or the <code>max-age</code> from the <code>expires</code> date, the server uses <code>getCurrentTime</code>. This means your system clock should be reasonably accurate. If your server is not synchronized using <code>NTP</code> or something similar it should be.</p>

<h4><a name="cookie_race">Cookie Updates are Not Atomic</a></h4>

<p>Cookie updates are not performed in any sort of atomic manner. As a result, the <a href="#simple_cookie_demo">simple cookie demo</a> contains a race condition. We get the <code>Cookie</code> value that was included in the <code>Request</code> and use it to create an updated <code>Cookie</code> value in the <code>Response</code>. But remember that the server can be processing many requests in parallel and the browser can make multiple requests in parallel. If the browser, for example, requested 10 images at once, they would all have the same initial cookie value. So, even though they all updated the counter by 1, they all started from the same value and ended with the same value. The count could even go backwards depending on the order <code>Requests</code> are received and <code>Responses</code> are processed.</p>


