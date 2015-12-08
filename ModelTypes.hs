module ModelTypes where

import Prelude
import Yesod

data BoxState = Available | Borrowed | Unavailable
    deriving (Show, Read, Eq)
derivePersistField "BoxState"
