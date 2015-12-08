module Handler.Game where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..),
                              withSmallInput)
import Data.List (cycle)



getShowGameDefaultR :: Handler Html
getShowGameDefaultR = getShowGameR NameAsc


getShowGameR :: OrderType -> Handler Html
getShowGameR order = do
    authId <- maybeAuthId
    let gameOrder = case order of
                        NameAsc        -> [Asc GameName]
                        NameDesc       -> [Desc GameName]
                        TimeAsc        -> [Asc GameTime, Asc GameName]
                        TimeDesc       -> [Desc GameTime, Asc GameName]
                        MinPlayersAsc  -> [Asc GameMinPlayers, Asc GameName]
                        MinPlayersDesc -> [Desc GameMinPlayers, Asc GameName]
                        OptPlayersAsc  -> [Asc GameOptimumPlayers, Asc GameName]
                        OptPlayersDesc -> [Desc GameOptimumPlayers, Asc GameName]
                        MaxPlayersAsc  -> [Asc GameMaxPlayers, Asc GameName]
                        MaxPlayersDesc -> [Desc GameMaxPlayers, Asc GameName]
                        _              -> [Asc GameName]   
    allGamesEntities <- runDB $ selectList [] gameOrder

    allBoxesEntities <- runDB $ selectList ([BoxState ==. Available] ||. [BoxState ==. Borrowed]) []

    let gamesToShow = case authId of
                          Just _  -> allGamesEntities
                          Nothing -> map (\(_, x) -> x) $ joinTables boxGame allBoxesEntities allGamesEntities
    
    let allGames = zip (cycle [0,1]) (map (\x -> (entityKey x, entityVal x)) gamesToShow)



    defaultLayout $ do
        setTitle "Lista gier"
        $(widgetFile "menu")
        $(widgetFile "table_css")
        $(widgetFile "showgame")

getAddGameR :: Handler Html
getAddGameR = do
    (formWidget, formEnctype) <- generateFormPost addGameForm
    addGamePage Nothing formWidget formEnctype

postAddGameR :: Handler Html
postAddGameR = do
    ((result, _), _) <- runFormPost addGameForm
    (formWidget, formEnctype) <- generateFormPost addGameForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    insertResult <- case submission of
                        Nothing -> return Nothing
                        Just game -> runDB $ insertUnique $ game
    addGamePage (Just insertResult) formWidget formEnctype


addGamePage formRun formWidget formEnctype = do
    authId' <- requireAuthId
    let authId = Just authId'

    defaultLayout $ do
        setTitle "Dodaj grę"
        $(widgetFile "menu")
        $(widgetFile "result_css")
        $(widgetFile "addgame")


addGameForm :: Html -> MForm Handler (FormResult Game, Widget)
addGameForm = renderBootstrap3 (BootstrapHorizontalForm  (ColXs 0) (ColXs 3) (ColXs 0) (ColXs 9)) $ Game
    <$> areq textField "Tytuł gry" Nothing
    <*> areq intField "Średni czas gry" Nothing
    <*> areq intField "Minimalna liczba graczy" Nothing
    <*> areq intField "Optymalna liczba graczy" Nothing
    <*> areq intField "Maksymalna liczba graczy" Nothing
    <*  bootstrapSubmit ("Dodaj grę" :: BootstrapSubmit Text)
