module Handler.Borrowing where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..))
import Data.List (cycle)

getBorrowingR :: Handler Html
getBorrowingR = borrowingsShow Nothing

postBorrowingEndR :: BorrowingId -> Handler Html
postBorrowingEndR borrowingId = do
    maybeBorrowing <- runDB $ get $ borrowingId
    case maybeBorrowing of
        Nothing -> borrowingsShow (Just $ selectErrorString)
        Just borrowing -> do
            now <- liftIO $ getCurrentTime
            runDB $ update (borrowingBox borrowing) [BoxState =. Available]
            runDB $ update borrowingId [BorrowingEnd =. Just now]
            borrowingsShow Nothing


borrowingsShow bFormRun = do
    let rFormRun = Nothing

    allGamesEntities <- runDB $ selectList [] []
    allBoxesEntities <- runDB $ selectList [] []
    activeBorrowingsEntities <- runDB $ selectList [] [Desc BorrowingStart]

    let activeBorrowingsCodesGames = zip (cycle ([0,1]::[Integer])) $ (map (\(x, y, z) -> ((entityKey x, entityVal x), entityVal y, entityVal z))) $ joinJoinTables borrowingBox boxGame activeBorrowingsEntities allBoxesEntities allGamesEntities

    authId <- maybeAuthId

    defaultLayout $ do
        setTitle "Wszystkie wypożyczenia"
        $(widgetFile "menu")
        $(widgetFile "result_css")
        $(widgetFile "table_css")
        $(widgetFile "showborrowings")


data Test = Test BorrowingId

formErrorString :: Text
formErrorString = "Błąd formularza. Spróbuj ponownie."
selectErrorString :: Text
selectErrorString = "Brak takiego wypożyczenia w bazie. Spróbuj ponownie."
