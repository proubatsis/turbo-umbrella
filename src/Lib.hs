{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import GHC.Generics

data InvoiceItem = InvoiceItem { item :: String
                                , hours :: Int
                                , rate :: Int
                                } deriving (Show,Generic)

instance FromJSON InvoiceItem
instance ToJSON InvoiceItem

x = InvoiceItem { item = "QA"
                , hours = 4
                , rate = 19 }

someFunc :: IO ()
someFunc = putStrLn $ show $ encode x
