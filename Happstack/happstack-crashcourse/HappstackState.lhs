<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" 
          "http://www.w3.org/TR/html4/strict.dtd">
<html>
 <head>
  <title>Crash Course in Happstack</title>
  <link type='text/css' rel='stylesheet' href='hscolour.css'>
  <link type='text/css' rel='stylesheet' href='theme/theme.css'>
 </head>
 <body>
<!--

> {-# LANGUAGE CPP #-}

-->
<p><a href="index.html">Back to Table of Contents</a></p>
<h1><a name="happstack_state">Happstack State</a></h1>
<p class="warning">This section will be expanded during the Happstack 7 development period.</p>

<p>This is a small app demonstrating the basics of using
happstack-state. This app just has a simple counter which can be
incremented or read.</p>

<p>First a bunch of <code>LANGUAGE</code> pragmas and imports:</p>

<div class="code">

> {-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
>   MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
>
> module Main where
>
> import Control.Applicative  ( (<$>))
> import Control.Exception    ( bracket)
> import Control.Monad        ( msum)
> import Control.Monad.Reader ( ask)
> import Control.Monad.State  ( get, put)
> import Data.Data            ( Data, Typeable)
> import Happstack.Server     ( Response, ServerPart, dir, nullDir, nullConf, ok
>                             , simpleHTTP, toResponse)
> import Happstack.State      ( Component(..), End, Proxy(..), Query, Update, Version
>                             , createCheckpoint, deriveSerialize, mkMethods 
>                             , query, startSystemState, shutdownSystem, update )
>

</div>

<p>Next we define a type that we wish to store in our state. In this case which just create a <code>newtype</code> wrapper around <code>Integer</code>:</p>

<div class="code">

> newtype Counter = Counter { unCounter :: Integer }
>     deriving (Eq, Num, Enum, Ord, Read, Show, Data, Typeable)
>
> instance Version Counter
> $(deriveSerialize ''Counter)
>

</div>

<p>The <code>Version</code> instance is used for migration. If we later change the <code>Counter</code> type, we will want to be able to migrate old saved data to the new format. Migration will be covered in a later section.</p>

<p><code>deriveSerialize</code> creates an instance of the <code>Serialize</code> class for <code>Counter</code>. The <code>Serialize</code> class specifies how to convert instances to binary representations and back. It also plays a role in the version migration process.</p>

<p>Next we will create a simple record that holds all the state for our application:</p>

<div class="code">

> data AppState = AppState {
>       count :: Counter
>     } deriving (Eq, Ord, Read, Show, Data, Typeable)
>
> instance Version AppState
> $(deriveSerialize ''AppState)
>

</div>

<p>It is a bit overkill for this application since we only have one piece of state that we are tracking. But in larger apps, it is a common pattern.</p>

<p>We then create an instance of <code>Component</code> for <code>AppState</code>.

<div class="code">

> instance Component AppState where
>     type Dependencies AppState = End
>     initialValue = AppState { count = 0 }
>

</div>

<p>A <code>Component</code> defines the boundries of transactions. An app can have multiple components, but each update or query can only see one component. Because the components operate independently, transactions in one component do not block transactions in another component. This can be used to provide better scaling in applications which perform many updates.</p>

<p><code>Dependencies</code> is used when we have multiple components. Here we only have one, so we specify that the dependency list is empty.</p>

<p><code>initialValue</code> specifies what the initial state of the database should be when the database is first created. Once the database has been created, changing that value has no effect.</p>


<p>Next we define an update function:</p>

<div class="code">

> addCounter :: Integer -> Update AppState Counter
> addCounter n =
>     do appState <- get
>        let newCount = (count appState) + (Counter n)
>        put $ appState { count = newCount } 
>        return newCount
>

</div>

<p>The <code>Update</code> monad is an enchanced version of the <code>State</code> monad. For the moment it is perhaps easiest to just pretend that <code>addCounter</code> has the type signature:</p>

<div class="code">
#ifdef HsColour
> addCounter :: Integer -> State AppState Counter
#endif
</div>

<p>And then it becomes clearer that <code>addCounter</code> is just a simple function in the <code>State</code> monad which updates <code>AppState</code>.</p>

<p>When the <code>addCounter</code> function is invoked, it will be run in an isolated manner (the 'I' in ACID). That means that you do not need to worry about some other thread modifying the <code>AppState</code> in between the <code>get</code> and the <code>put</code>.</p>

<p>You may also note that Update (and State) are not instances of the <code>MonadIO</code> class. This means you can not perform IO inside the update. This is by design. The Update monad does provide a few special IO functions related to getting the current time or generating a random number. Those functions are covered elsewhere.</p>

<p>We can also define a query which only reads the state, and does not update it:</p>

<div class="code">

> peekCounter :: Query AppState Counter
> peekCounter = count <$> ask
>

</div>

<p>The <code>Query</code> monad is an enhanced version of the <code>Reader</code> monad. So we can pretend that <code>peekCounter</code> has the type:</p>

<div class="code">
#ifdef HsColour
> peekCounter :: Reader AppState Counter
#endif
</div>

<p>Although we could have just used <code>get</code> in the <code>Update</code> monad, it is better to use the <code>Query</code> monad if you are doing a read-only operation because it will have much better performance. It also lets the user calling the function know that the database will not be affected.</p>

<p>Next we have to register those update and query functions with the <code>AppState</code> component:</p>

<div class="code">

> $(mkMethods ''AppState ['addCounter, 'peekCounter])
>

</div>

<p>Here we actually call our query and update functions:</p>

<div class="code">

> handlers :: ServerPart Response
> handlers = 
>     msum [ dir "peek" $ do c <- query PeekCounter
>                            ok $ toResponse $ "peeked at the count and saw: " ++ show (unCounter c)
>          , do nullDir 
>               c <- update (AddCounter 1)
>               ok $ toResponse $ "New count is: " ++ show (unCounter c)
>
>          ]
>

</div>

<p>Note that we do not call the functions directly. Instead we invoke them using the <code>update</code> and <code>query</code> functions:</p>

<div class="code">
#ifdef HsColour
> update :: (MonadIO m, UpdateEvent ev res) => ev -> m res
> query  :: (MonadIO m, QueryEvent  ev res) => ev -> m res
#endif
</div>

<p>Thanks to <code>mkMethods</code>, the functions that we originally defined now have types with the same name, but starting with an uppercase letter:</p>

<div class="code">
#ifdef HsColour
> data PeekCounter = PeekCounter
> data AddCounter  = AddCounter Integer
#endif
</div>

<p>The arguments to the constructors are the same as the arguments to the original function.</p>

<p>Finally, we have our main function:</p>

<div class="code">

> main :: IO ()
> main =
>     do bracket (startSystemState (Proxy :: Proxy AppState)) createCheckpointAndShutdown $ 
>          \_control ->
>            simpleHTTP nullConf handlers
>     where
>       createCheckpointAndShutdown control = 
>           do createCheckpoint control
>              shutdownSystem control

</div>

<p><code>startSystemState (Proxy :: Proxy AppState)</code> starts the transaction system. The <code>Proxy</code> argument tells the transaction system which <code>Component</code> we want to use as the top-level component.</p>

<p>The shutdown sequence creates a checkpoint when the server exits. This is good practice because it helps the server start faster, and makes migration go more smoothly. Calling <code>createCheckpoint</code> and <code>shutdownSystem</code> are not critical to data integrity. If the server crashes unexpectedly, it will replay all the logged transactions. But, running them during a normal shutdown sequence is a good idea.</p>


<p class="source-code">[Source code for the app is <a href="HappstackState.hs">here.</a>]</p>
<p><a href="WebRoutes.html">Next: web-routes</a></p>
<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script><script type="text/javascript"
    >var pageTracker = _gat._getTracker("UA-7111625-1");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
 </body>
</html>