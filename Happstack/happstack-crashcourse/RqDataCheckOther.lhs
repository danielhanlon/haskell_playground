<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="rqdatacheckrqmore">Other uses of <code>checkRq</code></a></h3>

<p>Looking again at the type for <code>checkRq</code> we see that
function argument is fairly general -- it is not restricted to just
string input:</p>

<div class="code">
#ifdef HsColour
> checkRq :: RqData a -> (a -> Either String b) -> RqData b
#endif
</div>

<p>So, <code>checkRq</code> is not limited to just parsing a <code>String</code> into a value. We could use it, for example, to validate an existing value. In the following example we use <code>lookRead "i"</code> to convert the value <code>i</code> to an <code>Int</code>, and then we use <code>checkRq</code> to ensure that the value is within range:</p>

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, checkRq, getDataFn, look, lookRead)
>
> data Vote = Yay | Nay deriving (Eq, Ord, Read, Show, Enum, Bounded)
>
> inRange :: (Show a, Ord a) => a -> a -> a -> Either String a
> inRange lower upper a
>     | lower <= a && a <= upper = Right a
>     | otherwise =
>         Left (show a ++ " is not between " ++ show lower ++ " and " ++ show upper)
>
> oneToTenPart :: ServerPart String
> oneToTenPart =
>     do r <- getDataFn (lookRead "i" `checkRq` (inRange 1 10))
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "You picked: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ oneToTenPart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataCheckOther.hs">here.</a>]</p>

<p>Now if we visit <a href="http://localhost:8000/?i=10">http://localhost:8000/?i=10</a>, we will get the message:

<pre class="code">
 $ curl http://localhost:8000/?i=10
You picked: 10
</pre>

<p>But if we pick an out of range value <a href="http://localhost:8000/?i=113">http://localhost:8000/?i=113</a>, we will get the message:

<pre class="code">
 $ curl http://localhost:8000/?i=113
113 is not between 1 and 10
</pre>
