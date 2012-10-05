<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="rqdatafromdata">Using the <code>FromData</code> class</a></h3>

<p>Sometimes the representation of a value as a request parameter will
be different from its representation expected by <code>Read</code>. In
these cases, we might instead use the <code>FromData</code>
class:</p>

<div class="code">
#ifdef HsColour
> class FromData a where
>     fromData :: RqData a
#endif
</div>

<p>In this example we create a type <code>Vote</code> with a <code>FromData</code> instance:</p>

<div class="code">

> module Main where
>
> import Control.Applicative ((<$>), (<*>))
> import Happstack.Server (ServerPart, badRequest, nullConf, ok, simpleHTTP)
> import Happstack.Server.RqData (RqData, lookRead, getDataFn)
>
> data Vote = Yay | Nay deriving (Eq, Ord, Read, Show, Enum, Bounded)
>
> instance FromData Vote where
>     fromData =
>         do s <- look ""
>
> lookInt :: RqData Int
> lookInt = lookRead "int"
>
> intPart :: ServerPart String
> intPart =
>     do r <- getDataFn myPolicy lookInt
>        case r of
>          (Left e) ->
>              badRequest $ unlines e
>          (Right i) ->
>              ok $ "Read the int: " ++ show i
>
> main :: IO ()
> main = simpleHTTP nullConf $ intPart

</div>
<p class="source-code">[Source code for the app is <a href="RqDataRead.hs">here.</a>]</p>

<p>Now if we visit <a href="http://localhost:8000/?int=1">http://localhost:8000/?int=1</a>, we will get the message:

<pre class="code">
Read the int: 1
</pre>

<p>If we visit <a href="http://localhost:8000/?int=apple">http://localhost:8000/?int=apple</a>, we will get the error:

<pre class="code">
Read failed while parsing: apple
</pre>
