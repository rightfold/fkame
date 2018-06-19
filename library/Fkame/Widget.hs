{-# LANGUAGE TemplateHaskell #-}

module Fkame.Widget
    ( Widget (..), widgetSource, widgetArguments
    , static
    , portal
    ) where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Network.URI (URI, uriToString)

import qualified Data.Aeson as AE
import qualified Data.Text as T

-- | Something the user can interact with.
data Widget =
    Widget
        { _widgetSource    :: Text
        , _widgetArguments :: Vector AE.Value }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

$(makeLenses ''Widget)

-- | Widget that renders static text.
static :: Text -> Widget
static text = Widget source arguments
    where
    source :: Text
    source = "var para = document.createElement('p');\n\
             \para.textContent = $1;\n\
             \$container.appendChild(para);"

    arguments :: Vector AE.Value
    arguments = [ AE.String text ]

-- | Widget that renders a textual hyperlink.
portal :: Text -> URI -> Widget
portal text uri = Widget source arguments
    where
    source :: Text
    source = "var link = document.createElement('a');\n\
             \link.textContent = $1;\n\
             \link.href = $2;\n\
             \$container.appendChild(link);"

    arguments :: Vector AE.Value
    arguments = [ AE.String text
                , AE.String . T.pack $ uriToString id uri "" ]
