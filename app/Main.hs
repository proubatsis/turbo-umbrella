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

getHarvestUsername = getEnv "TURBO_UMBRELLA_HARVEST_USERNAME"
getHarvestPassword = getEnv "TURBO_UMBRELLA_HARVEST_PASSWORD"
getHarvestOrganization = getEnv "TURBO_UMBRELLA_HARVEST_ORGANIZATION"
getSmptHost = getEnv "TURBO_UMBRELLA_SMTP_HOST"
getSmtpUsername = getEnv "TURBO_UMBRELLA_SMTP_USERNAME"
getSmtpPassword = getEnv "TURBO_UMBRELLA_SMTP_PASSWORD"
getSmtpFromEmail = getEnv "TURBO_UMBRELLA_SMTP_FROM_EMAIL"
getSmtpToEmail = getEnv "TURBO_UMBRELLA_SMTP_TO_EMAIL"
getLineItemTitle = getEnv "TURBO_UMBRELLA_LINE_ITEM_TITLE"
getEmailSubject = getEnv "TURBO_UMBRELLA_EMAIL_SUBJECT"
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
    let invoiceMaybe = do
            t <- timesheets
            let timesheetsByDay = zip days t
            let items = filter (\x -> (float2Double $ I.hours x) > 0.0) $ map (\(x, y) -> toLineItem lineItemTitle x y) timesheetsByDay
            return $ I.createInvoice "My Amazing Invoice" days items

    case invoiceMaybe of
        Just invoice -> do
                putStrLn "Saving invoice..." -- Save
                invoiceNumber <- I.saveInvoice "sample.db" invoice
                putStrLn $ "Invoice saved as " ++ (show invoiceNumber)
                putStrLn "Generating PDF..." -- PDF
                r <- I.renderInvoice (I.withInvoiceId invoiceNumber invoice) "invoice" "invoice-template/"
                writeFile "invoice.tex" $ TL.unpack r
                createProcess $ shell "pdflatex invoice.tex > pdflatex-invoice.log"
                putStrLn "Sending invoice..." -- Send
                sendEmail smtpHost smtpUsername smtpPassword $ createEmail smtpFromEmail smtpToEmail emailSubject emailBody ["invoice.pdf"]
        Nothing -> putStrLn "Failed to render invoice!"
