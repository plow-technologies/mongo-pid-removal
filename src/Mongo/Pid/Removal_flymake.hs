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
module Mongo.Pid.Removal (removeMissingAlarms)where


import Data.Text
import Control.Applicative 
import Data.Aeson 
import Persist.Mongo.Settings
import Database.Persist.MongoDB


removeMissingAlarms mdbc pidList = do
  
  --  print $ (toJSON(entityVal <$> lst))

