module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, BootstrapSubmit (..), bootstrapSubmit, BootstrapGridOptions (..),
                              withSmallInput)
import Data.List (cycle)



getAdminR :: Handler Html
getAdminR = do
    authId' <- requireAuthId
    let authId = Just authId'

    defaultLayout $ do
        setTitle "Menu administracyjne"
        $(widgetFile "menu")
        $(widgetFile "menuadmin")
