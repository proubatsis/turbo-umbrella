{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Invoice
    ( InvoiceLineItem(..)
    , Invoice(..)
    , createInvoice
    , newLineItem
    , renderInvoice
    , saveInvoice
    , withInvoiceId
    ) where

import Data.Time.Calendar
import Data.Aeson
import Text.Mustache
import GHC.Generics
import qualified Data.Text.Lazy as TL
import qualified Database.SQLite.Simple as DB

data InvoiceLineItem = InvoiceLineItem { title :: String
                                        , date :: Day
                                        , hours :: Float
                                        } deriving (Generic,Show)

data Invoice = Invoice { invoiceId :: Int
                        , invoiceTitle :: String
                        , startDate :: Day
                        , endDate :: Day
                        , invoiceItems :: [InvoiceLineItem]
                        } deriving (Generic,Show)

instance ToJSON InvoiceLineItem
instance ToJSON Invoice

createInvoice :: String -> [Day] -> [InvoiceLineItem] -> Invoice
createInvoice invoiceTitle days items = Invoice { invoiceId = 0
                                                , invoiceTitle = invoiceTitle
                                                , startDate = head days
                                                , endDate = last days
                                                , invoiceItems = items
                                                }

withInvoiceId :: Int -> Invoice -> Invoice
withInvoiceId newId inv =
    inv { invoiceId = newId }

newLineItem :: String -> Day -> Float -> InvoiceLineItem
newLineItem t d h = InvoiceLineItem { title = t
                                    , date = d
                                    , hours = h }

renderInvoice :: Invoice -> PName -> String -> IO TL.Text
renderInvoice invoice templateEntryFileName templateDirName = do
    template <- compileMustacheDir templateEntryFileName templateDirName
    let val = toJSON invoice
    return $ renderMustache template val

saveInvoice :: String -> Invoice -> IO Int
saveInvoice dbFile invoice = do
    let items = invoiceItems invoice
    conn <- DB.open dbFile
    DB.execute conn "INSERT INTO invoice (title, start_date, end_date) VALUES (?, ?, ?)" $ DB.toRow (invoiceTitle invoice :: String, startDate invoice :: Day, endDate invoice :: Day)
    invoiceId <- DB.lastInsertRowId conn >>= (\x -> return $ fromIntegral x)
    mapM (\x -> DB.execute conn "INSERT INTO invoice_item (invoice_id, title, work_date, hours, rate) VALUES (?, ?, ?, ?, ?)" $ DB.toRow (invoiceId :: Int, title x :: String, date x :: Day, hours x :: Float, 100 :: Float)) items
    DB.close conn
    return invoiceId
