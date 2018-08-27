{-# LANGUAGE RecordWildCards #-}

module LocalInventory where

import Data.Aeson

data LocalInventory = LocalInventory
  { localInventoryDummy :: ()
  }

instance ToJSON LocalInventory where
  toJSON LocalInventory{..} = object []
