<!--

> {-# LANGUAGE CPP #-}

-->

<h3><a name="FromReqURI"><code>FromReqURI</code>: extending <code>path</code></a></h3>

<p>We can extend path so that we can extract our own types from the path components as well. We simply add an instance to the FromReqURI class: </p>

<div class="code">
#ifdef HsColour
> class FromReqURI a where 
>     fromReqURI :: String -> Maybe a
#endif
</div>

<p>For example, let's say that we want to create a type to represent subjects we can greet.</p>
<div class="code">

> module Main where
>
> import Control.Monad (msum)
> import Data.Char (toLower)
> import Happstack.Server (FromReqURI(..), nullConf, simpleHTTP, ok, dir, path)
>
> data Subject = World | Haskell
>
> sayHello :: Subject -> String
> sayHello World   = "Hello, World!"
> sayHello Haskell = "Greetings, Haskell!"
>

</div>
<p>We simply add an instance such as: </p>
<div class="code">

> instance FromReqURI Subject where
>     fromReqURI sub =
>         case map toLower sub of
>           "haskell" -> Just Haskell
>           "world"   -> Just World
>           _         -> Nothing
>

</div>
<p>Now when we use <code>path</code> it will extract a value of type <code>Subject</code>.</p>
<div class="code">

> main :: IO ()
> main = simpleHTTP nullConf $ dir "hello" $ path $ \subject -> ok $ (sayHello subject)
>

</div>
<p class="source-code">[Source code for the app is <a href="FromReqURI.hs">here.</a>]</p>

<p>Now, if we start the app and point our browser at:  <a href="http://localhost:8000/hello/World">http://localhost:8000/hello/World</a> we get the "Hello, World"". </p>
<p>if we point it at <a href="http://localhost:8000/hello/Haskell">http://localhost:8000/hello/Haskell</a>, we get "Greetings, Haskell!".</p>