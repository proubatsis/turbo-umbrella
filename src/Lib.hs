{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

data InvoiceItem = InvoiceItem { item :: String
                                , hours :: Int
                                , rate :: Int
                                } deriving (Show,Generic)

instance FromJSON InvoiceItem
instance ToJSON InvoiceItem

x = InvoiceItem { item = "QA"
                , hours = 4
                , rate = 19 }

myFunc :: String -> Maybe InvoiceItem
myFunc s = decode $ B.pack s

someFunc :: IO ()
someFunc = do
    f <- readFile "sample.json"
    putStrLn $ show $ myFunc f
