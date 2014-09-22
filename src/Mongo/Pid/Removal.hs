{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Mongo.Pid.Removal
Description :  Get Pid from Name
Copyright   :  (c) Plow Technology 2014
License     :  MIT

Maintainer  :  brent.phillips@plowtech.net
Stability   :  unstable
Portability :  portable

<Uses a name to grab the pid to remove old pid alarms from mongo>
-}

{-# LANGUAGE OverloadedStrings, NoImplicitPrelude ,RecordWildCards #-}

module Mongo.Pid.Removal (removeMissingAlarms) where


import BasicPrelude hiding (delete)
import Persist.Mongo.Settings
import Database.Persist
import Data.Aeson
import Data.Traversable
removeMissingAlarms :: Either String MongoDBConf -> [Int] -> IO ()
removeMissingAlarms (Left st) _ = putStrLn "Missing mongo config" >> print st
removeMissingAlarms (Right mdbc) alarmListPids = do
                          vals <- runDBConf mdbc $ do
                            alarms <- selectList [] []
                            let filteredAlarms = filterAlarmByPids alarmListPids
                                                     <$> alarms
                            void $ traverse (delete.entityKey) (catMaybes filteredAlarms)
                          return()

filterAlarmByPids :: [Int] -> (Entity Alarm) -> Maybe (Entity Alarm)                          
filterAlarmByPids pids a@(Entity _ (Alarm {
                          alarmPids })) = const a <$> listToMaybe 
                                               [p2 | p1 <- pids, p2 <- alarmPids, p1 == p2] 
