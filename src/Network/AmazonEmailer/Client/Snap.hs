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

sendMessage :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessage e@(Email to from fromName subj body) =
  do liftIO $ print e
     void $ execute "insert into amazon_email_queue (to_addr, from_addr, from_name, subject, body) values (?,?,?,?,?)" (to, from, fromName, subj, body)

sendMessageHtml :: HasPostgres (Handler b a) => Email -> Handler b a ()
sendMessageHtml e@(Email to from fromName subj body) =
  do liftIO $ print e
     void $ execute "insert into amazon_email_queue (to_addr, from_addr, from_name, subject, body, html) values (?,?,?,?,?, true)" (to, from, fromName, subj, body)


sendMessages :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessages es = do
  liftIO $ putStrLn $ "Sending " ++ show (length es) ++ " messages."
  executeMany "insert into amazon_email_queue (to_addr,from_addr,from_name,subject,body) values (?,?,?,?,?)"
    (map (\(Email to from fromName subj body) -> (to,from,fromName,subj,body)) es)
  return ()

sendMessagesHtml :: HasPostgres (Handler b a) => [Email] -> Handler b a ()
sendMessagesHtml es = do
  liftIO $ putStrLn $ "Sending " ++ show (length es) ++ " messages."
  executeMany "insert into amazon_email_queue (to_addr,from_addr,from_name,subject,body,html) values (?,?,?,?,?,?, true)"
    (map (\(Email to from fromName subj body) -> (to,from,fromName,subj,body)) es)
  return ()
