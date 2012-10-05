<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="cookie_life">Cookie Lifetime</a></h3>

<p>When you set a cookie, you also specify the lifetime of that cookie. Cookies are referred to as <code>session cookies</code> or <code>permanent cookies</code> depending on how their lifetime is set.</p>

<dl>
 <dt>session cookie</dt>
 <dd>A cookie which expires when the browser is closed.</dd>
 <dt>permanent cookie</dt>
 <dd>A cookie which is saved (to disk) and is available even if the browser is restarted. The expiration time is set by the server.</dd>
</dl>

<p>The lifetime of a <code>Cookie</code> is specified using the <code>CookieLife</code> type:

<div class="code">
#ifdef HsColour
> -- | the lifetime of the cookie
> data CookieLife
>   = Session          -- ^ expire when the browser is closed
>   | MaxAge Seconds   -- ^ expire after the specified number of seconds
>   | Expires UTCTime  -- ^ expire at a specific date and time
>   | Expired          -- ^ expire immediately
#endif
</div>

<p>If you are intimately familiar with cookies, you may know that cookies have both an <code>expires</code> directive and a <code>max-age</code> directive, and wonder how they related to the constructors in <code>CookieLife</code>. Internet Explorer only supports the obsolete <code>expires</code> directive, instead of newer <code>max-age</code> directive. Most other browser will honor the <code>max-age</code> directive over <code>expires</code> if both are present. To make everyone happy, we always set both.</p>

<p>So, when setting <code>CookieLife</code> you can use <code>MaxAge</code> or <code>Expires</code> -- which ever is easiest, and the other directive will be calculated automatically.</p>

<h3><a name="deleting_a_cookie">Deleting a Cookie</a></h3>

<p>There is no explicit <code>Response</code> header to delete a cookie you have already sent to the client. But, you can convince the client to delete a cookie by sending a new version of the cookie with an expiration date that as already come and gone. You can do that by using the <code>Expired</code> constructor. Or, you can use the more convenient, <code>expireCookie</code> function.

<div class="code">
#ifdef HsColour
> -- | Expire the cookie immediately and set the cookie value to ""
> expireCookie :: (MonadIO m, FilterMonad Response m) => 
>                 String  -- ^ cookie name
>              -> m () 
#endif
</div>
