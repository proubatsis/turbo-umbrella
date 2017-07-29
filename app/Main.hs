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
import EMail

getHarvestUsername :: IO String
getHarvestUsername = getEnv "TURBO_UMBRELLA_HARVEST_USERNAME"

getHarvestPassword :: IO String
getHarvestPassword = getEnv "TURBO_UMBRELLA_HARVEST_PASSWORD"

getHarvestOrganization :: IO String
getHarvestOrganization = getEnv "TURBO_UMBRELLA_HARVEST_ORGANIZATION"

getSmptHost :: IO String
getSmptHost = getEnv "TURBO_UMBRELLA_SMTP_HOST"

getSmtpUsername :: IO String
getSmtpUsername = getEnv "TURBO_UMBRELLA_SMTP_USERNAME"

getSmtpPassword :: IO String
getSmtpPassword = getEnv "TURBO_UMBRELLA_SMTP_PASSWORD"

getSmtpFromEmail :: IO String
getSmtpFromEmail = getEnv "TURBO_UMBRELLA_SMTP_FROM_EMAIL"

getSmtpToEmail :: IO String
getSmtpToEmail = getEnv "TURBO_UMBRELLA_SMTP_TO_EMAIL"

getLineItemTitle :: IO String
getLineItemTitle = getEnv "TURBO_UMBRELLA_LINE_ITEM_TITLE"

getEmailSubject :: IO String
getEmailSubject = getEnv "TURBO_UMBRELLA_EMAIL_SUBJECT"

getEmailBody :: IO String
getEmailBody = getEnv "TURBO_UMBRELLA_EMAIL_BODY"

getPastTwoWeeks :: Day -> [Day]
getPastTwoWeeks day = [ addDays i $ addDays (-14) day | i <- [0..13] ]

stringToTextIO :: String -> IO TL.Text
stringToTextIO s = return $ TL.pack s

main :: IO ()
main = do
    harvestUsername <- getHarvestUsername
    harvestPassword <- getHarvestPassword
    harvestOrganization <- getHarvestOrganization
    smtpHost <- getSmptHost
    smtpUsername <- getSmtpUsername
    smtpPassword <- getSmtpPassword
    smtpFromEmail <- getSmtpFromEmail
    smtpToEmail <- getSmtpToEmail
    lineItemTitle <- getLineItemTitle
    emailSubject <- getEmailSubject
    emailBody <- getEmailBody >>= stringToTextIO

    UTCTime day time <- getCurrentTime
    
    putStrLn "Fetching timesheets..." -- Fetch
    let days = getPastTwoWeeks day
    timesheets <- getHarvestTimesheets harvestUsername harvestPassword harvestOrganization days
    
    putStrLn "Generating invoice..." -- Generate
    let renderedInvoiceMaybe = do
            t <- timesheets
            let timesheetsByDay = zip days t
            let items = filter (\x -> (float2Double $ I.hours x) > 0.0) $ map (\(x, y) -> toLineItem lineItemTitle x y) timesheetsByDay
            let renderedInvoice = I.renderInvoice items "invoice" "invoice-template/"
            return $ I.renderInvoice items "invoice" "invoice-template/"

    renderedInvoice <- sequence $ renderedInvoiceMaybe
    case renderedInvoice of Just r -> do
                                        writeFile "invoice.tex" $ TL.unpack r
                                        createProcess $ shell "pdflatex invoice.tex > pdflatex-invoice.log"
                                        putStrLn "Sending invoice..." -- Send
                                        sendEmail smtpHost smtpUsername smtpPassword $ createEmail smtpFromEmail smtpToEmail emailSubject emailBody ["invoice.pdf"]
                            Nothing -> putStrLn "Failed to render invoice!"
