module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..))
import Data.List (cycle)

getHomeR :: Handler Html
getHomeR = homePage Nothing Nothing

postHomeActionR :: BorrowingAction -> Handler Html
postHomeActionR action = 
    case action of
        Borrow -> borrowAction
        Return -> returnAction

borrowAction :: Handler Html
borrowAction = do
    ((result, _), _) <- runFormPost borrowForm

    let submission = case result of
                          FormSuccess res -> Just res
                          _ -> Nothing

    case submission of
        Just (BorrowSubmit boxText userText) -> do
            maybeBox <- runDB $ getBy $ UniqueBox boxText
            case maybeBox of
                Nothing -> homePage (Just Nothing) Nothing
                Just box -> do
                     case boxState $ entityVal $ box of
                         Available -> do
                              now <- liftIO $ getCurrentTime
                              runDB $ update (entityKey box) [BoxState =. Borrowed]
                              _ <- runDB $ insert $ Borrowing (entityKey box) userText now Nothing
                              homePage (Just Nothing) Nothing
                         _         -> homePage (Just $ Just stateErrorStringB) Nothing
        _ -> homePage (Just $ Just formErrorString) Nothing

returnAction :: Handler Html
returnAction = do
    ((result, _), _) <- runFormPost returnForm

    let submission = case result of
                          FormSuccess res -> Just res
                          _ -> Nothing

    case submission of
        Just (ReturnSubmit boxText) -> do
            maybeBox <- runDB $ getBy $ UniqueBox boxText
            case maybeBox of
                Nothing -> homePage Nothing (Just Nothing)
                Just box -> do
                     case boxState $ entityVal $ box of
                         Borrowed   -> do
                             now <- liftIO $ getCurrentTime
                             runDB $ update (entityKey box) [BoxState =. Available]
                             runDB $ updateWhere [BorrowingBox ==. (entityKey box), BorrowingEnd ==. Nothing ] [BorrowingEnd =. Just now]
                             homePage Nothing (Just Nothing)
                         _         -> homePage Nothing (Just $ Just $ stateErrorStringR)
        _ -> homePage Nothing (Just $ Just formErrorString)

homePage :: Maybe (Maybe Text) -> Maybe (Maybe Text) -> Handler Html
homePage bFormRun rFormRun = do
    (bFormWidget, bFormEnctype) <- generateFormPost borrowForm
    (rFormWidget, rFormEnctype) <- generateFormPost returnForm

    allGamesEntities <- runDB $ selectList [] []
    allBoxesEntities <- runDB $ selectList [] []
    activeBorrowingsEntities <- runDB $ selectList [BorrowingEnd ==. Nothing] [Desc BorrowingStart]

    let activeBorrowingsCodesGames = zip (cycle ([0,1]::[Integer])) $ (map (\(x, y, z) -> ((entityKey x, entityVal x), entityVal y, entityVal z))) $ joinJoinTables borrowingBox boxGame activeBorrowingsEntities allBoxesEntities allGamesEntities

    authId <- maybeAuthId

    defaultLayout $ do
        setTitle "Wypożycz/Zwróć"
        $(widgetFile "menu")
        $(widgetFile "result_css")
        $(widgetFile "borrowreturn")
        $(widgetFile "table_css")
        $(widgetFile "showborrowings")


data BorrowSubmit = BorrowSubmit Text Text
data ReturnSubmit = ReturnSubmit Text

borrowForm :: Html -> MForm Handler (FormResult BorrowSubmit, Widget)
borrowForm = renderBootstrap3 (BootstrapHorizontalForm  (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)) $ BorrowSubmit
    <$> areq textField "Kod pudełka" Nothing
    <*> areq textField "Kod uczestnika" Nothing
    <*  bootstrapSubmit ("Wypożycz" :: BootstrapSubmit Text)

returnForm :: Html -> MForm Handler (FormResult ReturnSubmit, Widget)
returnForm = renderBootstrap3 (BootstrapHorizontalForm  (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)) $ ReturnSubmit
    <$> areq textField "Kod pudełka" Nothing
    <*  bootstrapSubmit ("Zwróć" :: BootstrapSubmit Text)

formErrorString :: Text
formErrorString = "Błąd formularza. Spróbuj ponownie."
stateErrorStringB :: Text
stateErrorStringB = "Gra wypożyczona, lub niedostępna. Sprawdź stan i spróbuj ponownie."
stateErrorStringR :: Text
stateErrorStringR = "Gra nie jest wypożyczona. Sprawdź stan i spróbuj ponownie."
