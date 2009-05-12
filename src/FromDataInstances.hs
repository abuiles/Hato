module FromDataInstances where

import Happstack.Server
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Safe


data  TaskDescription = TaskDescription B.ByteString
                      

instance FromData TaskDescription where
    fromData = liftM ((TaskDescription).B.pack) (look "newTaskField" `mplus` return "")

data TaskIdFromForm = TIFF Integer 

instance FromData TaskIdFromForm where
    fromData = 
        liftM (TIFF) (do tid <- look "tid" `mplus` return "-1" ; safeRead tid)

data LastURL = LastURL String

instance FromData LastURL where
    fromData = 
        liftM LastURL (look "lasturl" `mplus` (return "/tasks/todo/"))

-- lifted from happs-tutotial
safeRead :: (Monad m, Read a) => [Char] -> m a
safeRead s = 
  maybe (fail $ "safeRead: " ++ s)
        return 
        (readMay s)