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

{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Mongo.Pid.Removal (removeMissingAlarms) where


import BasicPrelude 
import Persist.Mongo.Settings
import Database.Persist
import Data.Aeson

removeMissingAlarms :: Either String MongoDBConf -> [Int] -> IO ()
removeMissingAlarms (Left st) _ = putStrLn "Missing mongo config" >> print st
removeMissingAlarms (Right mdbc) alarmListPids = do
                          vals <- runDBConf mdbc $ do
                            pids <- selectList [OnpingTagCombinedPid <-. (Just <$> alarmListPids)] []
                            return pids
                          print $ toJSON vals
