> {-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
    >              TemplateHaskell, GADTs, FlexibleContexts,
    >              MultiParamTypeClasses #-}

    Now our imports.

    > import Yesod
    > import Yesod.Auth
    > import Yesod.Form.Nic (YesodNic, nicHtmlField)
    > import Yesod.Auth.BrowserId (authBrowserId)
    > import Data.Text (Text)
    > import Network.HTTP.Conduit (Manager, newManager, def)
    > import Database.Persist.Sqlite
    >     ( ConnectionPool, SqlPersist, runSqlPool, runMigration
            >     , createSqlitePool
            >     )
    > import Data.Time (UTCTime, getCurrentTime)
    > import Control.Applicative ((<$>), (<*>), pure)

    First we'll set up our Persistent entities. We're going to create both our 
    data types (via mkPersist) and a migration function, which will automatically
    create and update our SQL schema. If you were using the MongoDB backend,
    migration would not be needed.

    > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

    Keeps track of users. In a more robust application, we would also keep account
    creation date, display name, etc.

    > User
    >    email Text
    >    UniqueUser email

    An individual blog entry (I've avoided using the word "post" due to the
            confusion with the request method POST).

    > Entry
    >    title Text
    >    posted UTCTime
    >    content Html

    We need to tack on this "deriving" line since Html doesn't specify instances
    for Read, Show, or Eq. If you get an error message about "cannot derive" in your
        own code, try adding the deriving statement.

        >    deriving

        And a comment on the blog post.

        > Comment
        >    entry EntryId
        >    posted UTCTime
        >    user UserId
        >    name Text
        >    text Textarea
        > |]

        Every site has a foundation data type. This value is initialized before
        launching your application, and is available throughout. We'll store a database
        connection pool and HTTP connection manager in ours. See the very end of this
        file for how those are initialized.

        > data Blog = Blog
        >    { connPool :: ConnectionPool
            >    , httpManager :: Manager
                >    }

                To make i18n easy and translator friendly, we have a special file format for
                translated messages. There is a single file for each language, and each file is
                named based on the language code (e.g., en, es, de-DE) and placed in that
                folder. We also specify the main language file (here, "en") as a default
                language.

                > mkMessage "Blog" "../messages-blog" "en"

                Our en message file contains the following content:

                    NotAnAdmin: You must be an administrator to access this page.

                        WelcomeHomepage: Welcome to the homepage
                            SeeArchive: See the archive

                                NoEntries: There are no entries in the blog
                                    LoginToPost: Admins can login to post
                                        NewEntry: Post to blog
                                            NewEntryTitle: Title
                                                NewEntryContent: Content

                                                    PleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
                                                        EntryCreated title@Text: Your new blog post, #{title}, has been created

                                                            EntryTitle title@Text: Blog post: #{title}
                                                                CommentsHeading: Comments
                                                                    NoComments: There are no comments
                                                                        AddCommentHeading: Add a Comment
                                                                            LoginToComment: You must be logged in to comment
                                                                                AddCommentButton: Add comment

                                                                                    CommentName: Your display name
                                                                                        CommentText: Comment
                                                                                            CommentAdded: Your comment has been added
                                                                                                PleaseCorrectComment: Your submitted comment had some errors, please correct and try again.

                                                                                                    HomepageTitle: Yesod Blog Demo
                                                                                                        BlogArchiveTitle: Blog Archive

                                                                                                        Now we're going to set up our routing table. We have four entries: a homepage,
                                                                                                        an entry list page (BlogR), an individual entry page (EntryR) and our
                                                                                                        authentication subsite. Note that BlogR and EntryR both accept GET and POST
                                                                                                        methods. The POST methods are for adding a new blog post and adding a new
                                                                                                        comment, respectively.

                                                                                                        > mkYesod "Blog" [parseRoutes|
                                                                                                        > / RootR GET
                                                                                                        > /blog BlogR GET POST
                                                                                                        > /blog/#EntryId EntryR GET POST
                                                                                                        > /auth AuthR Auth getAuth
                                                                                                        > |]

                                                                                                        Every foundation needs to be an instance of the Yesod typeclass. This is where
                                                                                                        we configure various settings.

                                                                                                        > instance Yesod Blog where

                                                                                                        The base of our application. Note that in order to make BrowserID work
                                                                                                        properly, this must be a valid URL.

                                                                                                        >     approot = ApprootStatic "http://localhost:3000"

                                                                                                        Our authorization scheme. We want to have the following rules:

                                                                                                        * Only admins can add a new entry.
                                                                                                        * Only logged in users can add a new comment.
                                                                                                        * All other pages can be accessed by anyone.

                                                                                                        We set up our routes in a RESTful way, where the actions that could make
                                                                                                        changes are always using a POST method. As a result, we can simply check for
                                                                                                        whether or not a request is a write request, given by the True in the second
                                                                                                        field.

                                                                                                        First, we'll authorize requests to add a new entry.

                                                                                                        >     isAuthorized BlogR True = do
                                                                                                        >         mauth <- maybeAuth
                                                                                                        >         case mauth of
                                                                                                        >             Nothing -> return AuthenticationRequired
                                                                                                        >             Just (Entity _ user)
    >                 | isAdmin user -> return Authorized
    >                 | otherwise    -> unauthorizedI MsgNotAnAdmin

    Now we'll authorize requests to add a new comment.

    >     isAuthorized (EntryR _) True = do
    >         mauth <- maybeAuth
    >         case mauth of
    >             Nothing -> return AuthenticationRequired
    >             Just _  -> return Authorized

    And for all other requests, the result is always authorized.

    >     isAuthorized _ _ = return Authorized

    Where a user should be redirected to if they get an AuthenticationRequired.

    >     authRoute _ = Just (AuthR LoginR)

    This is where we define our site look-and-feel. The function is given the
    content for the individual page, and wraps it up with a standard template.

    >     defaultLayout inside = do

    Yesod encourages the get-following-post pattern, where after a POST, the user
    is redirected to another page. In order to allow the POST page to give the user
    some kind of feedback, we have the getMessage and setMessage functions. It's a
    good idea to always check for pending messages in your defaultLayout function.

    >         mmsg <- getMessage

    We use widgets to compose together HTML, CSS, and JavaScript. At the end of the
    day, we need to unwrap all of that into simple HTML. That's what the
    widgetToPageContent function is for. We're going to give it a widget consisting
    of the content we received from the individual page (inside), plus a standard
    CSS for all pages. We'll use the Lucius template language to create the latter.

    >         pc <- widgetToPageContent $ do
    >             toWidget [lucius|
    > body {
        >     width: 760px;
        >     margin: 1em auto;
        >     font-family: sans-serif;
        > }
        > textarea {
            >     width: 400px;
            >     height: 200px;
            > }
            > #message {
                >   color: #900;
                > }
                > |]
                >             inside

                And finally we'll use a new Hamlet template to wrap up the individual
                components (title, head data and body data) into the final output.

                >         hamletToRepHtml [hamlet|
                > $doctype 5
                > <html>
                >     <head>
                >         <title>#{pageTitle pc}
                >         ^{pageHead pc}
                >     <body>
                >         $maybe msg <- mmsg
                >             <div #message>#{msg}
                >         ^{pageBody pc}
                > |]

                This is a simple function to check if a user is the admin. In a real
                application, we would likely store the admin bit in the database itself, or
                check with some external system. For now, I've just hardcoded my own email
                address.

                > isAdmin :: User -> Bool
                > isAdmin user = userEmail user == "michael@snoyman.com"

                In order to access the database, we need to create a YesodPersist instance,
                which says which backend we're using and how to run an action.

                > instance YesodPersist Blog where
                >    type YesodPersistBackend Blog = SqlPersist
                >    runDB f = do 
                >        master <- getYesod
                >        let pool = connPool master
                >        runSqlPool f pool

                This is a convenience synonym. It is defined automatically for you in the
                scaffolding.

                > type Form x = Html -> MForm Blog Blog (FormResult x, Widget)

    In order to use yesod-form and yesod-auth, we need an instance of RenderMessage
    for FormMessage. This allows us to control the i18n of individual form
    messages.

    > instance RenderMessage Blog FormMessage where
    >     renderMessage _ _ = defaultFormMessage

    In order to use the built-in Nic HTML editor, we need this instance. We just
    take the default values, which use a CDN-hosted version of Nic.

    > instance YesodNic Blog

    In order to use yesod-auth, we need a YesodAuth instance.

    > instance YesodAuth Blog where
    >     type AuthId Blog = UserId
    >     loginDest _ = RootR
    >     logoutDest _ = RootR
    >     authHttpManager = httpManager

    We'll use [BrowserID](https://browserid.org/), which is a third-party system
    using email addresses as your identifier. This makes it easy to switch to other
    systems in the future, such as locally authenticated email addresses (also included
            with yesod-auth).

    >     authPlugins _ = [authBrowserId]

    This function takes someone's login credentials (i.e., his/her email address)
    and gives back a user ID.

    >     getAuthId creds = do
    >         let email = credsIdent creds
    >             user = User email
    >         res <- runDB $ insertBy user
    >         return $ Just $ either entityKey id res

    Homepage handler. The one important detail here is our usage of `setTitleI`,
    which allows us to use i18n messages for the title. We also use this message
    with a `_{Msg...}` interpolation in Hamlet.

    > getRootR :: Handler RepHtml
    > getRootR = defaultLayout $ do
    >     setTitleI MsgHomepageTitle
    >     [whamlet|
    > <p>_{MsgWelcomeHomepage}
    > <p>
    >    <a href=@{BlogR}>_{MsgSeeArchive}
    > |]

    Define a form for adding new entries. We want the user to provide the title and
    content, and then fill in the post date automatically via `getCurrentTime`.

    > entryForm :: Form Entry
    > entryForm = renderDivs $ Entry
    >     <$> areq textField (fieldSettingsLabel MsgNewEntryTitle) Nothing
    >     <*> aformM (liftIO getCurrentTime)
    >     <*> areq nicHtmlField (fieldSettingsLabel MsgNewEntryContent) Nothing

    Get the list of all blog entries, and present an admin with a form to create a
    new entry.

    > getBlogR :: Handler RepHtml
    > getBlogR = do
    >     muser <- maybeAuth
    >     entries <- runDB $ selectList [] [Desc EntryPosted]
    >     ((_, entryWidget), enctype) <- generateFormPost entryForm
    >     defaultLayout $ do
    >         setTitleI MsgBlogArchiveTitle
    >         [whamlet|
    > $if null entries
    >     <p>_{MsgNoEntries}
    > $else
    >     <ul>
    >         $forall Entity entryId entry <- entries
    >             <li>
    >                 <a href=@{EntryR entryId}>#{entryTitle entry}

    We have three possibilities: the user is logged in as an admin, the user is
    logged in and is not an admin, and the user is not logged in. In the first
    case, we should display the entry form. In the second, we'll do nothing. In the
    third, we'll provide a login link.

    > $maybe Entity _ user <- muser
    >     $if isAdmin user
    >         <form method=post enctype=#{enctype}>
    >             ^{entryWidget}
    >             <div>
    >                 <input type=submit value=_{MsgNewEntry}>
    > $nothing
    >     <p>
    >         <a href=@{AuthR LoginR}>_{MsgLoginToPost}
    > |]

    Process an incoming entry addition. We don't do any permissions checking, since
    isAuthorized handles it for us. If the form submission was valid, we add the
    entry to the database and redirect to the new entry. Otherwise, we ask the user
    to try again.

    > postBlogR :: Handler RepHtml
    > postBlogR = do
    >     ((res, entryWidget), enctype) <- runFormPost entryForm
    >     case res of
    >         FormSuccess entry -> do
    >             entryId <- runDB $ insert entry
    >             setMessageI $ MsgEntryCreated $ entryTitle entry
    >             redirect $ EntryR entryId
    >         _ -> defaultLayout $ do
    >             setTitleI MsgPleaseCorrectEntry
    >             [whamlet|
    > <form method=post enctype=#{enctype}>
    >     ^{entryWidget}
    >     <div>
    >         <input type=submit value=_{MsgNewEntry}>
    > |]

    A form for comments, very similar to our entryForm above. It takes the
    EntryId of the entry the comment is attached to. By using pure, we embed
    this value in the resulting Comment output, without having it appear in the
    generated HTML.

    > commentForm :: EntryId -> Form Comment
    > commentForm entryId = renderDivs $ Comment
    >     <$> pure entryId
    >     <*> aformM (liftIO getCurrentTime)
    >     <*> aformM requireAuthId
    >     <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    >     <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing

    Show an individual entry, comments, and an add comment form if the user is
    logged in.

    > getEntryR :: EntryId -> Handler RepHtml
    > getEntryR entryId = do
    >     (entry, comments) <- runDB $ do
    >         entry <- get404 entryId
    >         comments <- selectList [] [Asc CommentPosted]
    >         return (entry, map entityVal comments)
    >     muser <- maybeAuth
    >     ((_, commentWidget), enctype) <-
    >         generateFormPost (commentForm entryId)
    >     defaultLayout $ do
    >         setTitleI $ MsgEntryTitle $ entryTitle entry
    >         [whamlet|
    > <h1>#{entryTitle entry}
    > <article>#{entryContent entry}
    >     <section .comments>
    >         <h1>_{MsgCommentsHeading}
    >         $if null comments
    >             <p>_{MsgNoComments}
    >         $else
    >             $forall Comment _entry posted _user name text <- comments
    >                 <div .comment>
    >                     <span .by>#{name}
    >                     <span .at>#{show posted}
    >                     <div .content>#{text}
    >         <section>
    >             <h1>_{MsgAddCommentHeading}
    >             $maybe _ <- muser
    >                 <form method=post enctype=#{enctype}>
    >                     ^{commentWidget}
    >                     <div>
    >                         <input type=submit value=_{MsgAddCommentButton}>
    >             $nothing
    >                 <p>
    >                     <a href=@{AuthR LoginR}>_{MsgLoginToComment}
    > |]

    Receive an incoming comment submission.

    > postEntryR :: EntryId -> Handler RepHtml
    > postEntryR entryId = do
    >     ((res, commentWidget), enctype) <-
    >         runFormPost (commentForm entryId)
    >     case res of
    >         FormSuccess comment -> do
    >             _ <- runDB $ insert comment
    >             setMessageI MsgCommentAdded
    >             redirect $ EntryR entryId
    >         _ -> defaultLayout $ do
    >             setTitleI MsgPleaseCorrectComment
    >             [whamlet|
    > <form method=post enctype=#{enctype}>
    >     ^{commentWidget}
    >     <div>
    >         <input type=submit value=_{MsgAddCommentButton}>
    > |]

    Finally, our main function.

    > main :: IO ()
    > main = do
    >     pool <- createSqlitePool "blog.db3" 10 -- create a new pool
    >     -- perform any necessary migration
    >     runSqlPool (runMigration migrateAll) pool
    >     manager <- newManager def -- create a new HTTP manager
    >     warpDebug 3000 $ Blog pool manager -- start our server
