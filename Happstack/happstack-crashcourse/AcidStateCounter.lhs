<h2><a name="acid_state_counter"><kbd>acid-state</kbd> counter</a></h2>

<p>Our first example is a very simple hit counter app.</p>

<p>First a bunch of <code>LANGUAGE</code> pragmas and imports:</p>

<div class="code">

> {-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
>   MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}
>
> module Main where
>
> import Control.Applicative  ( (<$>) )
> import Control.Exception    ( bracket )
> import Control.Monad        ( msum )
> import Control.Monad.Reader ( ask )
> import Control.Monad.State  ( get, put )
> import Data.Data            ( Data, Typeable )
> import Happstack.Server     ( Response, ServerPart, dir, nullDir, nullConf, ok
>                             , simpleHTTP, toResponse )
> import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
> import Data.Acid.Advanced   ( query', update' )
> import Data.Acid.Local      ( createCheckpointAndClose )
> import Data.SafeCopy        ( base, deriveSafeCopy )

</div>

<p>Next we define a type that we wish to store in our state. In this case we just create a simple record with a single field <code>count</code>:</p>

<div class="code">

> data CounterState = CounterState { count :: Integer }
>     deriving (Eq, Ord, Read, Show, Data, Typeable)
>
> $(deriveSafeCopy 0 'base ''CounterState)
>

</div>

<p><code>deriveSafeCopy</code> creates an instance of the <code>SafeCopy</code> class for <code>CounterState</code>. <code>SafeCopy</code> is class for versioned serialization, deserilization, and migration. The <code>SafeCopy</code> class is a bit like a combination of the <code>Read</code> and <code>Show</code> classes, except that it converts the data to a compact <code>ByteString</code> representation, and it includes version information in case the type changes and old data needs to be migrated.</p>

<p>Since this is the first version of the <code>CounterState</code> type, we give it version number 0 and declare it to be the <code>base</code> type. Later if we change the type, we will increment the version to 1 and declare it to be an <code>extension</code> of a previous type. We will also provide a migration instance to migrate the old type to the new type. The migration will happen automatically when the old state is read. For more information on <code>SafeCopy, base, extension</code> and migration see the <a href="http://hackage.haskell.org/packages/archive/safecopy/0.6.1/doc/html/Data-SafeCopy.html">haddock docs</a>. (A detailed section on migration for the Crash Course is planned, but not yet written).</p>

<p>If you are not familiar with Template Haskell be sure to read this <a href="TemplateHaskell.html">brief intro to Template Haskell</a></p>

<p>Next we will define an initial value that is suitable for initializing the <code>CounterState</code> state.</p>

<div class="code">

> initialCounterState :: CounterState
> initialCounterState = CounterState 0

</div>

<p>Now that we have our types, we can define some update and query functions.</p>

<p>First let's define an update function which increments the count and returns the incremented value:</p>

<div class="code">

> incCountBy :: Integer -> Update CounterState Integer
> incCountBy n =
>     do c@CounterState{..} <- get
>        let newCount = count + n
>        put $ c { count = newCount }
>        return newCount
>

</div>

<p>In this line:</p>

<div class="code">
#ifdef HsColour
> c@CounterState{..} <- get
#endif
</div>

<p>we are using the <code>RecordWildCards</code> extension. The <code>{..}</code> binds all the fields of the record to symbols with the same name. That is why in the next line we can just write <code>count</code> instead of <code>(count c)</code>. Using <code>RecordWildCards</code> here is completely optional, but tends to make the code less cluttered, and easier to read.</p>

<p>Also notice that we are using the <code>get</code> and <code>put</code> functions from <code>MonadState</code> to get and put the ACID state. The <code>Update</code> monad is basically an enchanced version of the <code>State</code> monad. For the moment it is perhaps easiest to just pretend that <code>incCountBy</code> has the type signature:</p>

<div class="code">
#ifdef HsColour
> incCountBy :: Integer -> State CounterState Integer
#endif
</div>

<p>And then it becomes clearer that <code>incCountBy</code> is just a simple function in the <code>State</code> monad which updates <code>CounterState</code> and returns an <code>Integer</code>.</p>

<p>Note that even though we are using a monad here.. the code is still pure. If we wanted we could have required the update function to have a type like this instead:</p>

<div class="code">
#ifdef HsColour
> incCountBy :: Integer -> CounterState -> (CounterState, Integer)
#endif
</div>

<p>In that version, the current state is explicitly passed in, and the function explicitly returns the updated state. The monadic version does the same thing, but uses <code>&gt;&gt;=</code> to make the plumbing easier. This makes the monadic version easier to read and reduces mistakes.</p>

<p>When we later use the <code>update</code> function to call <code>incCountBy</code>, <code>incCountBy</code> will be run in an isolated manner (the 'I' in ACID). That means that you do not need to worry about some other thread modifying the <code>CounterState</code> between the <code>get</code> and the <code>put</code>. It will also be run atomically (the 'A' in ACID), meaning that either the whole function will run or it will not run at all. If the server is killed mid-transaction, the transaction will either be completely applied or not applied at all.</p>

<p>You may also note that <code>Update</code> (and <code>State</code>) are not instances of the <code>MonadIO</code> class. This means you can not perform IO inside the update. This is by design. In order to ensure Durability and to support replication, events need to be pure. That allows us to be confident that if the event log has to be replayed -- it will result in the same state we had before.

<p>We can also define a query which reads the state, and does not update it:</p>

<div class="code">

> peekCount :: Query CounterState Integer
> peekCount = count <$> ask
>

</div>

<p>The <code>Query</code> monad is an enhanced version of the <code>Reader</code> monad. So we can pretend that <code>peekCount</code> has the type:</p>

<div class="code">
#ifdef HsColour
> peekCount :: Reader CounterState Integer
#endif
</div>

<p>Although we could have just used <code>get</code> in the <code>Update</code> monad, it is better to use the <code>Query</code> monad if you are doing a read-only operation because it will not block other database transactions. It also lets the user calling the function know that the database will not be affected.</p>

<p>Next we have to turn the update and query functions into acid-state events. This is almost always done by using the template haskell function <code>makeAcidic</code></p>

<div class="code">

> $(makeAcidic ''CounterState ['incCountBy, 'peekCount])
>

</div>

<p>The <code>makeAcidic</code> function creates a bunch of boilerplate types and type class instances. If you want to see what is happening under the hood, check out the examples <a href="http://mirror.seize.it/acid-state/examples/">here</a>. The examples with names like, <kbd>HelloWorldNoTH.hs</kbd> show how to implement the boilerplate by hand. In practice, you will probably never want to or need to do this. But you may find it useful to have a basic understanding of what is happening. You could also use the <code>-ddump-splices</code> flag to ghc to see the auto-generated instances -- but the lack of formatting makes it difficult to read.</p>

<p>Here we actually call our query and update functions:</p>

<div class="code">

> handlers :: AcidState CounterState -> ServerPart Response
> handlers acid =
>     msum [ dir "peek" $ do c <- query' acid PeekCount
>                            ok $ toResponse $ "peeked at the count and saw: " ++ show c
>          , do nullDir
>               c <- update' acid (IncCountBy 1)
>               ok $ toResponse $ "New count is: " ++ show c
>
>          ]
>

</div>

<p>Note that we do not call the <code>incCountBy</code> and <code>peekCount</code> functions directly. Instead we invoke them using the <code>update'</code> and <code>query'</code> functions:</p>

<div class="code">
#ifdef HsColour
> update' :: (UpdateEvent event, MonadIO m) =>
>            AcidState (EventState event) -- ^ handle to acid-state
>         -> event                        -- ^ update event to execute
>         -> m (EventResult event)
> query'  :: (QueryEvent event , MonadIO m) =>
>            AcidState (EventState event) -- ^ handle to acid-state
>         -> event                        -- ^ query event to execute
>         -> m (EventResult event)
#endif
</div>

<p>Thanks to <code>makeAcidic</code>, the functions that we originally defined now have types with the same name, but starting with an uppercase letter:</p>

<div class="code">
#ifdef HsColour
> data PeekCount  = PeekCount
> data IncCountBy = IncCountBy Integer
#endif
</div>

<p>The arguments to the constructors are the same as the arguments to the original function.</p>

<p>So now we can decipher the meaning of the type for the <code>update'</code> and <code>query'</code> functions. For example, in this code:</p>

<div class="code">
#ifdef HsColour
> c <- update' acid (IncCountBy 1)
#endif
</div>

<p>The event is <code>(IncCountBy 1)</code> which has the type <code>IncCountBy</code>. Since there is an <code>UpdateEvent IncCountBy</code> instance, we can use this event with the <code>update'</code> function. That gives us:</p>

<div class="code">
#ifdef HsColour
> update' :: (UpdateEvent IncCountBy, MonadIO m) =>
>            AcidState (EventState IncCountBy)
>         -> IncCountBy
>         -> m (EventResult IncCountBy)
#endif
</div>

<p><code>EventState</code> is a type function. <code>EventState IncCountBy</code> results in the type <code>CounterState</code>. So that reduces to <code>AcidState CounterState</code>. So, we see that we can not accidently call the <code>IncCountBy</code> event against an acid state handle of the wrong type.</p>

<p><code>EventResult</code> is also a type function. <code>EventResult IncCountBy</code> is <code>Integer</code>, as we would expect from the type signature for <code>IncCountBy</code>.</p>

<p>As mentioned earlier, the underlying update and query events we created are pure functions. But, in order to have a durable database (aka, be able to recover after powerloss, etc) we do need to log these pure events to disk so that we can reply them in the event of a recovery. So, rather than invoke our update and query events directly, we call them indirectly via the <code>update</code> and <code>query</code> functions. <code>update</code> and <code>query</code> interact with the <code>acid-state</code> system to ensure that the acid-state events are properly logged, called in the correct order, run atomitically and isolated, etc.</p>

<p>There is no way in Haskell to save a function to save a function to disk or send it over the network. So, <kbd>acid-state</kbd> has to cheat a little. Instead of storing the function, it just stores the name of the function and the value of its arguments. That is what the <code>IncCountBy</code> type is for -- it is the value that can be serialized and saved to disk or sent over the network.</p>

<p>Finally, we have our main function:</p>

<div class="code">

> main :: IO ()
> main =
>     do bracket (openLocalState initialCounterState)
>                (createCheckpointAndClose)
>                (\acid ->
>                     simpleHTTP nullConf (handlers acid))

</div>

<p><code>openLocalState</code> starts up <kbd>acid-state</kbd> and returns an handle. If existing state is found on the disk, it will be automatically restored and used. If no pre-existing state is found, then <code>initialCounterState</code> will be used. <code>openLocalState</code> stores data in a directory named <kbd>state/[typeOf state]</kbd>. In this example, that would be, <kbd>state/CounterState</kbd>. If you want control over where the state information is stored use <code>openLocalStateFrom</code> instead.</p>

<p>The shutdown sequence creates a checkpoint when the server exits. This is good practice because it helps the server start faster, and makes migration go more smoothly. Calling <code>createCheckpointAndClose</code> is not critical to data integrity. If the server crashes unexpectedly, it will replay all the logged transactions (Durability). However, it is a good idea to create a checkpoint on close. If you change an existing update event, and then tried to replay old versions of the event, things would probably end poorly. However, restoring from a checkpoint does not require the old events to be replayed. Hence, always creating a checkpoint on shutdown makes it easier to upgrade the server.</p>

<p class="source-code">[Source code for the app is <a href="AcidStateCounter.hs">here.</a>]</p>

