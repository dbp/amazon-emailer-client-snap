{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.AmazonEmailer.Client.Snap where

import Data.Text (Text)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)

import Snap.Snaplet (Handler)
import Snap.Snaplet.PostgresqlSimple (HasPostgres, execute, executeMany)


data Email = Email { emTo :: Text
                   , emFrom :: Text
                   , emFromName :: Text
                   , emSubject :: Text
                   , emBody :: Text
                   } deriving (Show, Eq)

-- | Sends a single plain text message
sendMessage :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessage e@(Email to from fromName subj body) =
  void $ execute "insert into amazon_email_queue (to_addr, from_addr, from_name, subject, body) values (?,?,?,?,?)" (to, from, fromName, subj, body)

-- | Sends a single plain text message, writing it out to the stdout.
sendMessageVerbose :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessageVerbose e@(Email to from fromName subj body) =
  do liftIO $ print e
     sendMessage e

-- | Sends a single html message
sendMessageHtml :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessageHtml e@(Email to from fromName subj body) =
  void $ execute "insert into amazon_email_queue (to_addr, from_addr, from_name, subject, body, html) values (?,?,?,?,?, true)" (to, from, fromName, subj, body)

-- | Sends a single html message, writing it out to the stdout
sendMessageHtmlVerbose :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessageHtmlVerbose e@(Email to from fromName subj body) =
  do liftIO $ print e
     sendMessageHtml e

-- | Sends many plain text messages
sendMessages :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessages es = do
  executeMany "insert into amazon_email_queue (to_addr,from_addr,from_name,subject,body) values (?,?,?,?,?)"
    (map (\(Email to from fromName subj body) -> (to,from,fromName,subj,body)) es)
  return ()

-- | Sends many plain text messages, logging the result to stdout
sendMessagesVerbose :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessagesVerbose es = do
  liftIO $ putStrLn $ "Sending " ++ show (length es) ++ " messages."
  sendMessages es

-- | Sends many html messages
sendMessagesHtml :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessagesHtml es = do
  executeMany "insert into amazon_email_queue (to_addr,from_addr,from_name,subject,body,html) values (?,?,?,?,?,?, true)"
    (map (\(Email to from fromName subj body) -> (to,from,fromName,subj,body)) es)
  return ()

-- | Sends many html messages, logging the result to stdout
sendMessagesHtmlVerbose :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessagesHtmlVerbose es = do
  liftIO $ putStrLn $ "Sending " ++ show (length es) ++ " messages."
  sendMessagesHtml es
