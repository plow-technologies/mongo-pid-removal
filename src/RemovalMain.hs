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
import Mongo.Pid.Removal
import BasicPrelude 
import Persist.Mongo.Settings

lst :: [Int]
lst = [13536,13537,13538,13556,13780,13781,13782,14228,14229,14230,19683,19702,19703,21785,21804,21805,29099,29119,32011,32031,32197,32218,32219] 

main :: IO() 
main = do          
  conf <- readDBConf "mongoConfig.yml"
  removeMissingAlarms conf lst
