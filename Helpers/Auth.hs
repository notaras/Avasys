{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers.Auth
    ( authAvasysDummy
    ) where

import Yesod.Auth
import Yesod.Form (runInputPost, textField, ireq)
import Text.Hamlet (hamlet)
import Yesod.Core
import Prelude

authAvasysDummy :: YesodAuth m => AuthPlugin m
authAvasysDummy =
    AuthPlugin "avasysDummy" dispatch login
  where
    dispatch "POST" [] = lift $ setCredsRedirect $ Creds "avasysDummy" "Admin" []
    dispatch _ _ = notFound
    url = PluginR "avasysDummy" []
    login authToMaster =
        toWidget [hamlet|
$newline never
<form method="post" action="@{authToMaster url}">
    <h1>Wejdź w tryb administratora:
    <br>
    <button class="btn  btn-default" type="submit">Wejdź

|]


