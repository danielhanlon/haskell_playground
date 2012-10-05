<!--

> {-# LANGUAGE CPP #-}

-->

<h2><a name="rqdatalimiting">Limiting lookup to QUERY_STRING or request body</a></h2>

<p>By default, <code>look</code> and friends will search both the
QUERY_STRING the request body (aka, POST/PUT data) for a key. But
sometimes we want to specify that only the QUERY_STRING or request
body should be searched. This can be done by using the <code>body</code> and <code>queryString</code> filters:</p>

<div class="code">
#ifdef HsColour
> body :: (HasRqData m) => m a -> m a
> queryString ::  (HasRqData m) => m a -> m a
#endif
</div>

<p>Using these filters we can modify <code>helloPart</code> so that the <code>greeting</code> must come from the QUERY_STRING and the <code>noun</code> must come from the request body:</p>

<div class="code">
#ifdef HsColour
> helloPart :: ServerPart String
> helloPart =
>     do greeting <- queryString $ look "greeting"
>        noun     <- body        $ look "noun"
>        ok $ greeting ++ ", " ++ noun
#endif
</div>

<p><code>queryString</code> and <code>body</code> act as filters which
only pass a certain subset of the data through. If you were to
write:</p>

<div class="code">
#ifdef HsColour
> greetingRq :: ServerPart String
> greetingRq =
>     body (queryString $ look "greeting")
#endif
</div>

<p>This code would never match anything because the
<code>body</code> filter would hide all the QUERY_STRING values, and
the <code>queryString</code> filter would hide all the request body
values, and hence, there would be nothing left to search.</p>
