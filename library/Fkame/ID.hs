module Fkame.ID
    ( ID, _ID
    , mkID
    , unID
    ) where

import Control.Lens (Prism', prism')
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData)

import qualified Data.Text as Text

newtype ID =
    ID Text
    deriving stock (Generic, Eq, Ord, Show)
    deriving newtype (IsString, FromHttpApiData)
    deriving anyclass (ToJSON, ToJSONKey)

_ID :: Prism' Text ID
_ID = prism' unID mkID
{-# INLINE _ID #-}

mkID :: Text -> Maybe ID
mkID i | Text.null i = Nothing
       | Text.any (`notElem` safe) i = Nothing
       | otherwise = Just (ID i)
{-# INLINABLE mkID #-}

unID :: ID -> Text
unID (ID i) = i
{-# INLINE unID #-}

safe :: [Char]
safe = ['a' .. 'z'] <> ['-']
{-# INLINE safe #-}
