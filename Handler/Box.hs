module Handler.Box where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..),
                              withSmallInput)
import Data.List (cycle)
import qualified Data.Text as T
import Text.Read


getShowBoxR :: Maybe GameId -> Maybe OwnerId -> Maybe BoxState -> Handler Html
getShowBoxR gameId ownerId state = do
    allGamesEntities <-  runDB $ selectList (case gameId of
                                                Just gId -> [GameId ==. gId]
                                                _        -> []               ) [Asc GameName]
    allOwnersEntities <- runDB $ selectList (case ownerId of
                                                Just oId -> [OwnerId ==. oId]
                                                _        -> []               ) [Asc OwnerName]
    allBoxesEntities <- runDB $ selectList (case state of
                                                Just s -> [BoxState ==. s]
                                                _      -> []               ) [Asc BoxIdCode]

    let allBoxes = zip (cycle [0,1]) $ (map (\(x, y, z) -> ((entityKey x, entityVal x), entityVal y, entityVal z))) (joinTables3 boxGame boxOwner allBoxesEntities allGamesEntities allOwnersEntities)

    authId <- maybeAuthId

    defaultLayout $ do
        setTitle "Lista gier"
        $(widgetFile "menu")
        $(widgetFile "table_css")
        $(widgetFile "showbox")

postShowBoxR :: Maybe GameId -> Maybe OwnerId -> Maybe BoxState -> Handler Html
postShowBoxR gameId ownerId state = do
    boxIdMaybe <- lookupPostParam "boxId"
    ownerIdMaybe <- lookupPostParam "ownerId"
    boxStateMaybe <- lookupPostParam "boxState"

    let boxStateNew = case boxStateMaybe of
                          Just state -> read $ T.unpack state
                          _          -> Unavailable

    case boxIdMaybe of
        Nothing    -> return ()
        Just boxId -> runDB $ update (read $ T.unpack boxId :: BoxId) [BoxState =. boxStateNew]

    case ownerIdMaybe of
        Nothing    -> return ()
        Just ownerId -> runDB $ updateWhere [BoxOwner ==. (read $ T.unpack ownerId :: OwnerId)] [BoxState =. boxStateNew]

    getShowBoxR gameId ownerId state

getShowBoxDefaultR :: Handler Html
getShowBoxDefaultR = getShowBoxR Nothing Nothing Nothing

getAddBox1R :: Handler Html
getAddBox1R = addBox1 Nothing

getAddBox2R :: GameId -> Handler Html
getAddBox2R gameId = do
    authId' <- requireAuthId
    let authId = Just authId'

    allOwnersEntities <- runDB $ selectList [] [Asc OwnerName]
    let allOwners = zip (cycle [0,1]) (map (\x -> (entityKey x, entityVal x)) allOwnersEntities)

    defaultLayout $ do
        setTitle "Dodaj pudełko"
        $(widgetFile "menu")
        $(widgetFile "table_css")
        $(widgetFile "addbox2")

getAddBox3R :: GameId -> OwnerId -> Handler Html
getAddBox3R gameId ownerId = do
    (formWidget, formEnctype) <- generateFormPost $ addBoxForm ownerId gameId

    authId' <- requireAuthId
    let authId = Just authId'

    defaultLayout $ do
        setTitle "Dodaj grę"
        $(widgetFile "menu")
        $(widgetFile "addbox3")

postAddBox3R :: GameId -> OwnerId -> Handler Html
postAddBox3R gameId ownerId = do
    ((result, _), _) <- runFormPost $ addBoxForm ownerId gameId

    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    insertResult <- case submission of
                        Nothing -> return Nothing
                        Just box -> runDB $ insertUnique $ box
    addBox1 (Just insertResult)


addBox1 formRun = do
    allGamesEntities <- runDB $ selectList [] [Asc GameName]
    let allGames = zip (cycle [0,1]) (map (\x -> (entityKey x, entityVal x)) allGamesEntities)

    authId' <- requireAuthId
    let authId = Just authId'

    defaultLayout $ do
        setTitle "Dodaj pudełko"
        $(widgetFile "menu")
        $(widgetFile "table_css")
        $(widgetFile "result_css")
        $(widgetFile "addbox1")

addBoxForm :: OwnerId -> GameId -> Html -> MForm Handler (FormResult Box, Widget)
addBoxForm ownerId gameId = renderBootstrap3 (BootstrapHorizontalForm  (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)) $ Box
    <$> areq textField "Kod pudełka" Nothing
    <*> pure ownerId
    <*> pure gameId
    <*> pure Available
    <*  bootstrapSubmit ("Dodaj pudełko" :: BootstrapSubmit Text)

showState :: BoxState -> Text
showState Available = "Dostępna"
showState Borrowed = "Wypożyczona"
showState Unavailable = "Niedostępna"

otherState :: BoxState -> Text
otherState Available = "Unavailable"
otherState Borrowed = "Available"
otherState Unavailable = "Available"
