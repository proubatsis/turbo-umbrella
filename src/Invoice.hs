{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Invoice
    ( InvoiceLineItem(..)
    , newLineItem
    , renderInvoice
    , saveInvoice
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

instance ToJSON InvoiceLineItem

newLineItem :: String -> Day -> Float -> InvoiceLineItem
newLineItem t d h = InvoiceLineItem { title = t
                                    , date = d
                                    , hours = h }

renderInvoice :: [InvoiceLineItem] -> PName -> String -> IO TL.Text
renderInvoice items templateEntryFileName templateDirName = do
    template <- compileMustacheDir templateEntryFileName templateDirName
    let val = toJSON items
    return $ renderMustache template val

saveInvoice :: String -> [InvoiceLineItem] -> IO Int
saveInvoice dbFile items = do
    conn <- DB.open dbFile
    DB.execute conn "INSERT INTO invoice (title) VALUES (?)" $ DB.Only ("My Invoice" :: String)
    invoiceId <- DB.lastInsertRowId conn >>= (\x -> return $ fromIntegral x)
    mapM (\x -> DB.execute conn "INSERT INTO invoice_item (invoice_id, title, work_date, hours, rate) VALUES (?, ?, ?, ?, ?)" $ DB.toRow (invoiceId :: Int, title x :: String, date x :: Day, hours x :: Float, 100 :: Float)) items
    DB.close conn
    return invoiceId
