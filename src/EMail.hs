{-# LANGUAGE OverloadedStrings #-}

module EMail
    ( createEmail
    , sendEmail
    ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.Lazy as BL
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.Auth
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (Text,fromStrict)
import qualified Data.Text as T

data Email = Email { emailTo :: String
                    , emailFrom :: String
                    , emailSubject :: String
                    , emailContent :: Text
                    , emailAttachments :: [FilePath]
                    }
 
readEncodedBinaryFile :: FilePath -> IO BS.ByteString
readEncodedBinaryFile file = BS.readFile file >>= (\s -> return $ BL.encode s)

createEmail :: String -> String -> String -> Text -> [FilePath] -> Email
createEmail to from subject content attachmentFiles = Email { emailTo = to
                                                            , emailFrom = from
                                                            , emailSubject = subject
                                                            , emailContent = content
                                                            , emailAttachments = attachmentFiles }


sendEmail :: String -> UserName -> Password -> Email -> IO ()
sendEmail hostName username password email = doSMTPSSLWithSettings hostName defaultSettingsSMTPSSL $ \conn -> do
    authSucceeded <- authenticate PLAIN username password conn
    let content = emailContent email
    let from = emailFrom email
    let to = emailTo email
    let subject = emailSubject email
    let attachments = zip (map T.pack $ emailAttachments email) (emailAttachments email)

    if authSucceeded
        then sendMimeMail to from subject content content attachments conn
        else putStrLn "Email Authentication Failed!"
