module Invoice
    ( InvoiceLineItem(..)
    , newLineItem
    ) where

import Data.Time.Calendar

data InvoiceLineItem = InvoiceLineItem { title :: String
                                        , date :: Day
                                        , hours :: Float
                                        } deriving (Show)

newLineItem :: String -> Day -> Float -> InvoiceLineItem
newLineItem t d h = InvoiceLineItem { title = t
                                    , date = d
                                    , hours = h }
