{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Harvest
    ( fetchDay
    , HarvestResponse
    ) where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as C

data HarvestDayEntry = HarvestDayEntry { project :: String
                                        , hours :: Float
                                        } deriving (Generic,Show)

data HarvestResponse = HarvestResponse { day_entries :: [HarvestDayEntry]
                                        } deriving (Generic,Show)

instance FromJSON HarvestDayEntry
instance FromJSON HarvestResponse

fetchDayString :: String -> String -> String -> Int -> Integer -> IO B.ByteString
fetchDayString username password organization day year = do
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest $ "https://" ++ organization ++ ".harvestapp.com/daily/" ++ (show day) ++ "/" ++ (show year)
    let creq = applyBasicAuth (C.pack username) (C.pack password) $ initReq {
        requestHeaders =
            [ ("Accept", "application/json")
            ]
    }

    resp <- runResourceT $ httpLbs creq manager
    return $ responseBody resp

fetchDay :: String -> String -> String -> Day -> IO (Maybe HarvestResponse)
fetchDay username password organization day = do
    let (year, dayOfYear) = toOrdinalDate day
    s <- fetchDayString username password organization dayOfYear year
    return $ decode s
