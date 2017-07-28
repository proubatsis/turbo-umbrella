{-# LANGUAGE DeriveGeneric #-}

module Invoice
    ( InvoiceLineItem(..)
    , newLineItem
    , renderInvoice
    ) where

import Data.Time.Calendar
import Data.Aeson
import Text.Mustache
import GHC.Generics
import qualified Data.Text.Lazy as TL

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
