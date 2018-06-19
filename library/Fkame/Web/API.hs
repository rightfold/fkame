module Fkame.Web.API
    ( Monads
    , ContentTypes

    , RootAPI
    , rootServer

    , DashboardsAPI
    , dashboardsServer

    , PanelsAPI
    , panelsServer
    ) where

import Prelude hiding (show)

import Control.Lens (ix, preview, to, view)
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Data.Map.Strict (Map)
import Servant.API ((:<|>) (..), (:>), Capture, Get, JSON)
import Servant.Server (ServantErr, ServerT, err404)

import Fkame.Dashboard (Dashboard, DashboardID, dashboardPanels)
import Fkame.Panel (Panel, PanelID)



type Monads f =
    ( MonadError ServantErr f
    , MonadReader (Map DashboardID Dashboard) f )

type ContentTypes =
    '[JSON]



type RootAPI =
    "dashboards" :> DashboardsAPI

rootServer :: Monads f => ServerT RootAPI f
rootServer = dashboardsServer



type DashboardsAPI =
    Get ContentTypes (Map DashboardID Dashboard) :<|>
    Capture "dashboardID" DashboardID :> Get ContentTypes Dashboard :<|>
    Capture "dashboardID" DashboardID :> "panels" :> PanelsAPI

dashboardsServer :: forall f. Monads f => ServerT DashboardsAPI f
dashboardsServer = index :<|> show :<|> panels
    where
    index :: f (Map DashboardID Dashboard)
    index = view id

    show :: DashboardID -> f Dashboard
    show dashboardID =
        fromMaybe404 <=< preview $
            ix dashboardID

    panels :: DashboardID -> ServerT PanelsAPI f
    panels = panelsServer



type PanelsAPI =
    Get ContentTypes (DashboardID, Map PanelID Panel) :<|>
    Capture "panelID" PanelID :> Get ContentTypes (DashboardID, Panel)

panelsServer :: forall f. Monads f => DashboardID -> ServerT PanelsAPI f
panelsServer dashboardID = index :<|> show
    where
    index :: f (DashboardID, Map PanelID Panel)
    index = fromMaybe404 <=< preview $
        ix dashboardID . dashboardPanels . to ((,) dashboardID)

    show :: PanelID -> f (DashboardID, Panel)
    show panelID = fromMaybe404 <=< preview $
        ix dashboardID . dashboardPanels . ix panelID . to ((,) dashboardID)



fromMaybe404 :: MonadError ServantErr f => Maybe a -> f a
fromMaybe404 = maybe (throwError err404) pure
