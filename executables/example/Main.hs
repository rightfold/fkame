module Main
    ( main
    ) where

import Control.Monad.Reader (runReaderT)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Servant.Server (Server, hoistServer, serve)

import qualified Data.Map.Strict as Map
import qualified Network.Wai.Handler.Warp as Warp

import Fkame.Dashboard (Dashboard (..), DashboardID)
import Fkame.Web.API (RootAPI, rootServer)
import Fkame.Panel (Panel (..), PanelID)
import Fkame.Widget (static)

main :: IO ()
main =
    let
        dashboards :: Map DashboardID Dashboard
        dashboards = Map.fromList
            [ ("revenue", revenueDashboard) ]

        revenueDashboard :: Dashboard
        revenueDashboard = Dashboard "Revenue" panels
            where
            panels :: Map PanelID Panel
            panels = Map.fromList
                [ ("total", totalPanel) ]

            totalPanel :: Panel
            totalPanel = Panel $ static "$3,000,000,000"

        api :: Proxy RootAPI
        api = Proxy

        server :: Server RootAPI
        server = hoistServer api interpret rootServer
            where interpret = runReaderT `flip` dashboards
    in
        Warp.run 8083 $ serve api server
