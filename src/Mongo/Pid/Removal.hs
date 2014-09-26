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
import qualified Data.Set as S


  


removeMissingAlarms :: Either String MongoDBConf -> IO ()
removeMissingAlarms (Left st)  = putStrLn "Missing mongo config" >> print st
removeMissingAlarms (Right mdbc) = do
                            (joinKeys,alarmEntityKeys) <- runDBConf mdbc $ do
                              alarms <- selectList [] []
                              let pidlist = concat $ alarmPids.entityVal <$> alarms
                              otclist <- selectList [OnpingTagCombinedPid <-. (Just <$> pidlist)] []
                              let badPidSet = makeCheckSets pidlist (catMaybes $  (onpingTagCombinedPid.entityVal <$> otclist))
                              let badPidLst = S.toList badPidSet
                              let filteredAlarms = filterAlarmByPids badPidLst <$> alarms
                              let filteredAlarmEntities = (catMaybes filteredAlarms)
                              let alarmentitykeys = entityKey <$> filteredAlarmEntities
                              void $ traverse (delete.entityKey) (filteredAlarmEntities)
                              joinkeys <- selectKeysList [AlarmJoinsAlarmId <-. alarmentitykeys] []
                              void $ traverse delete joinkeys
                              return(joinkeys,alarmentitykeys)
                            putStrLn $ show.encode $ joinKeys
                            putStrLn $ show.encode $ alarmEntityKeys

filterAlarmByPids :: [Int] -> (Entity Alarm) -> Maybe (Entity Alarm)                          
filterAlarmByPids pids a@(Entity _ (Alarm {
                          alarmPids })) = const a <$> listToMaybe 
                                               [p2 | p1 <- pids, p2 <- alarmPids, p1 == p2] 
                     
  

makeCheckSets ::(Eq a, Ord a) =>  [a] -> [a] -> (S.Set a)
makeCheckSets l1 l2 = let s1 = S.fromList l1
                          s2 = S.fromList l2
                      in (S.difference s1 s2)
   
