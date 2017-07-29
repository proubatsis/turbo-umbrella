{-# LANGUAGE OverloadedStrings #-}

module Main where

import Harvest
import qualified Invoice as I
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment
import GHC.Float
import qualified Data.Text.Lazy as TL
import System.Process

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
    
    let renderedInvoiceMaybe = do
            t <- timesheets
            let timesheetsByDay = zip days t
            let items = filter (\x -> (float2Double $ I.hours x) > 0.0) $ map (\(x, y) -> toLineItem "MyTitle!" x y) timesheetsByDay
            return $ I.renderInvoice items "invoice" "invoice-template/"

    renderedInvoice <- sequence $ renderedInvoiceMaybe
    case renderedInvoice of Just r -> do
                                        writeFile "invoice.tex" $ TL.unpack r
                                        createProcess $ shell "pdflatex invoice.tex"
                                        return $ ()
                            Nothing -> putStrLn "Failed to render invoice!"
