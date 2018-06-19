{-# LANGUAGE TemplateHaskell #-}

module Fkame.Panel
    ( PanelID (..), _PanelID
    , Panel (..), panelWidget
    ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (ToJSON, ToJSONKey)
import Data.String (IsString)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData)

import Fkame.ID (ID)
import Fkame.Widget (Widget)

newtype PanelID =
    PanelID ID
    deriving stock (Generic, Eq, Ord, Show)
    deriving newtype (IsString, FromHttpApiData)
    deriving anyclass (ToJSON, ToJSONKey)

-- | Wrapper for a widget, including metadata.
newtype Panel =
    Panel
        { _panelWidget :: Widget }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

$(makePrisms ''PanelID)
$(makeLenses ''Panel)
