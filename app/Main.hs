module Main where

import Harvest
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment

getUsername :: IO String
getUsername = getEnv "HARVEST_USERNAME"

getPassword :: IO String
getPassword = getEnv "HARVEST_PASSWORD"

getOrganization :: IO String
getOrganization = getEnv "HARVEST_ORGANIZATION"

getPastTwoWeeks :: Day -> [Day]
getPastTwoWeeks day = [ addDays i $ addDays (-14) day | i <- [0..13] ]

getHarvestTimesheets :: String -> String -> String -> [Day] -> IO [Maybe HarvestResponse]
getHarvestTimesheets username password organization days = mapM (fetchDay username password organization) days

main :: IO ()
main = do
    username <- getUsername
    password <- getPassword
    organization <- getOrganization
    UTCTime day time <- getCurrentTime
    d <- getHarvestTimesheets username password organization $ getPastTwoWeeks day
    putStrLn $ show d
