{-# LANGUAGE NoMonomorphismRestriction,ScopedTypeVariables  #-}
module ControllerGetActions where

import StateVersions.AppState
import qualified Data.ByteString.Char8 as B 
import View
import Happstack.Helpers
import Happstack.State
import Happstack.Server
import Control.Monad
import Control.Monad.Reader
import Text.StringTemplate.Helpers 
import Text.StringTemplate
import Control.Monad.Trans



-- True for done, False to-do 
viewTasksByStatus :: Bool ->  (STGroup [Char]) ->  String  -> ServerPartT IO Response
viewTasksByStatus status stgr url = do  
  tasks <- query $ AskTasksByStatus status  
  let p  = length tasks
  let layout = pageLayout stgr [("contentarea",(paintTasksTable tasks url))]  
  return.toResponse.HtmlString$layout
  
--Test
addT = do 
  t <- query $ NewTask "Add more tests" False
  b <- update $ AddTask t
  return b
                  
           


type Status = Bool

paintTasksTable ::[Task] -> String ->  String
paintTasksTable tasks url = paintTable (Just ["Task","Status",""])(map (\(Task tid desc status)-> [B.unpack (desc),checkBox status tid,deleteButton tid url])  tasks) Nothing
