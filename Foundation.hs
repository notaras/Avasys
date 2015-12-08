module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import qualified Data.Text as T
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Helpers.Auth
import Yesod.Auth.Dummy

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

-- instance (PathPiece a) => PathPiece (Maybe a) where
--    fromPathPiece s = case s of
--        "Nothing" -> Nothing
--        _         -> Just $ fromPathPiece s
--    toPathPiece m = case m of
--        Just s -> toPathPiece s
--        _      -> "Nothing"


data BorrowingAction = Borrow | Return
    deriving(Show, Read, Eq)

instance PathPiece BorrowingAction where
    toPathPiece Borrow = T.pack $ "Borrow"
    toPathPiece Return = T.pack $ "Return" 

    fromPathPiece s = case T.unpack $ s of
                        "Return" -> Just Return
                        "Borrow" -> Just Borrow
                        _        -> Nothing  


data OrderType = NameAsc | NameDesc | TimeAsc | TimeDesc | MinPlayersAsc | MinPlayersDesc | OptPlayersAsc | OptPlayersDesc | MaxPlayersAsc | MaxPlayersDesc deriving(Show, Read, Eq)
instance PathPiece OrderType where
    toPathPiece NameAsc        = T.pack $ "NameAsc"
    toPathPiece NameDesc       = T.pack $ "NameDesc" 
    toPathPiece TimeAsc        = T.pack $ "TimeAsc"
    toPathPiece TimeDesc       = T.pack $ "TimeDesc"
    toPathPiece MinPlayersAsc  = T.pack $ "MinPlayersAsc"
    toPathPiece MinPlayersDesc = T.pack $ "MinPlayersDesc"
    toPathPiece OptPlayersAsc  = T.pack $ "OptPlayersAsc"
    toPathPiece OptPlayersDesc = T.pack $ "OptPlayersDesc"
    toPathPiece MaxPlayersAsc  = T.pack $ "MaxPlayersAsc"
    toPathPiece MaxPlayersDesc = T.pack $ "MaxPlayersDesc"
    toPathPiece _ = T.pack $ "NameAsc"

    fromPathPiece s = case T.unpack $ s of
                        "NameAsc"        -> Just NameAsc
                        "NameDesc"       -> Just NameDesc
                        "TimeAsc"        -> Just TimeAsc
                        "TimeDesc"       -> Just TimeDesc
                        "MinPlayersAsc"  -> Just MinPlayersAsc
                        "MinPlayersDesc" -> Just MinPlayersDesc
                        "OptPlayersAsc"  -> Just OptPlayersAsc
                        "OptPlayersDesc" -> Just OptPlayersDesc
                        "MaxPlayersAsc"  -> Just MaxPlayersAsc
                        "MaxPlayersDesc" -> Just MaxPlayersDesc
                        _          -> Nothing  


instance PathPiece BoxState where
    toPathPiece Available   = T.pack $ "Available"
    toPathPiece Borrowed    = T.pack $ "Borrowed" 
    toPathPiece Unavailable = T.pack $ "Unavailable"

    fromPathPiece s = case T.unpack $ s of
                        "Available"   -> Just Available
                        "Borrowed"    -> Just Borrowed
                        "Unavailable" -> Just Unavailable
                        _          -> Nothing  



data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = Text
--    getAuthId = return . Just . credsIdent
    maybeAuthId = lookupSession "_ID"

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False --True


    authenticate = return . Authenticated . credsIdent

    authPlugins _ = [authAvasysDummy]

    authHttpManager = getHttpManager




--instance YesodAuthPersist App where
--    type AuthEntity App = Text --PriviligeMode
-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
