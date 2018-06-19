{-# LANGUAGE TemplateHaskell #-}

module Fkame.Dashboard
    ( DashboardID (..), _DashboardID
    , Dashboard (..), dashboardTitle, dashboardPanels
    ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.String (IsString)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData)

import Fkame.ID (ID)
import Fkame.Panel (Panel, PanelID)

newtype DashboardID =
    DashboardID ID
    deriving stock (Generic, Eq, Ord, Show)
    deriving newtype (IsString, FromHttpApiData)
    deriving anyclass (ToJSON, ToJSONKey)

-- | Collection of panels.
data Dashboard =
    Dashboard
        { _dashboardTitle :: Text
        , _dashboardPanels :: Map PanelID Panel }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

$(makePrisms ''DashboardID)
$(makeLenses ''Dashboard)
