module Main where

import Harvest
import qualified Invoice as I
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import GHC.Float

getUsername :: IO String
getUsername = getEnv "HARVEST_USERNAME"

getPassword :: IO String
getPassword = getEnv "HARVEST_PASSWORD"

getOrganization :: IO String
getOrganization = getEnv "HARVEST_ORGANIZATION"

getPastTwoWeeks :: Day -> [Day]
getPastTwoWeeks day = [ addDays i $ addDays (-14) day | i <- [0..13] ]

main :: IO ()
main = do
    username <- getUsername
    password <- getPassword
    organization <- getOrganization
    UTCTime day time <- getCurrentTime
    
    let days = getPastTwoWeeks day
    timesheets <- getHarvestTimesheets username password organization days
    
    let lineItems = do
            t <- timesheets
            let timesheetsByDay = zip days t
            let items = map (\(x, y) -> toLineItem "MyTitle!" x y) timesheetsByDay
            return $ filter (\x -> (float2Double $ I.hours x) > 0.0) items

    putStrLn $ show $ lineItems
