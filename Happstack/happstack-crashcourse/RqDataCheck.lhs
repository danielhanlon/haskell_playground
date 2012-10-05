<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="rqdatacheckrq">Using <code>checkRq</code></a></h3>

<p>Sometimes the representation of a value as a request parameter will be different from the representation required by <code>Read</code>. We can use <code>checkRq</code> to lift a custom parsing function into <code>RqData</code>.</p>

<div class="code">
#ifdef HsColour
> checkRq :: (Monad m, HasRqData m) => m a -> (a -> Either String b) -> m b
#endif
</div>

<p>In this example we create a type <code>Vote</code> with a custom parsing function:</p>

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, checkRq, getDataFn, look, lookRead)
>
> data Vote = Yay | Nay deriving (Eq, Ord, Read, Show, Enum, Bounded)
>
> parseVote :: String -> Either String Vote
> parseVote "yay" = Right Yay
> parseVote "nay" = Right Nay
> parseVote str   = Left $ "Expecting 'yay' or 'nay' but got: " ++ str
>
> votePart :: ServerPart String
> votePart =
>     do r <- getDataFn (look "vote" `checkRq` parseVote)
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "You voted: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ votePart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataCheck.hs">here.</a>]</p>

<p>Now if we visit <a href="http://localhost:8000/?vote=yay">http://localhost:8000/?vote=yay</a>, we will get the message:

<pre class="code">
You voted: Yay
</pre>

<p>If we visit <a href="http://localhost:8000/?vote=yes">http://localhost:8000/?vote=yes</a>, we will get the error:

<pre class="code">
Expecting 'yay' or 'nay' but got: yes
</pre>
