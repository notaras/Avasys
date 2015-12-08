module Handler.Owner where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..),
                              withSmallInput)

import Data.List (cycle)

getShowOwnerR :: Handler Html
getShowOwnerR = do
    allOwnersEntities <- runDB $ selectList [] [Asc OwnerName]
    let allOwners = zip (cycle [0,1]) (map (\x -> (entityKey x, entityVal x)) allOwnersEntities)

    authId <- maybeAuthId

    defaultLayout $ do
        setTitle "Lista właścicieli"
        $(widgetFile "menu")
        $(widgetFile "table_css")
        $(widgetFile "showowner")


getAddOwnerR :: Handler Html
getAddOwnerR = do
    (formWidget, formEnctype) <- generateFormPost addOwnerForm

    addOwnerPage Nothing formWidget formEnctype    

postAddOwnerR :: Handler Html
postAddOwnerR = do
    ((result, _), _) <- runFormPost addOwnerForm
    (formWidget, formEnctype) <- generateFormPost addOwnerForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    insertResult <- case submission of
                        Nothing -> return Nothing
                        Just owner -> runDB $ insertUnique $ owner
    addOwnerPage (Just insertResult) formWidget formEnctype    

addOwnerPage formRun formWidget formEnctype = do
    authId' <- requireAuthId
    let authId = Just authId'

    defaultLayout $ do
        setTitle "Dodaj właściciela"
        $(widgetFile "menu")
        $(widgetFile "result_css")
        $(widgetFile "addowner")

addOwnerForm :: Html -> MForm Handler (FormResult Owner, Widget)
addOwnerForm = renderBootstrap3 (BootstrapHorizontalForm  (ColXs 0) (ColXs 2) (ColXs 0) (ColXs 10))$ Owner
    <$> areq textField "Nazwa" Nothing
    <*> aopt textField "Numer telefonu" Nothing
    <*> areq textareaField "Adres" Nothing
    <*  bootstrapSubmit ("Dodaj właściciela" :: BootstrapSubmit Text)
