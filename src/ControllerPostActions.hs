module ControllerPostActions where  

import Happstack.Server
import Happstack.Helpers
import Happstack.State
import Control.Monad
import FromDataInstances
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans
import ControllerGetActions
import StateVersions.AppState
import Text.StringTemplate

templatesGroups ::IO (STGroup [Char])
templatesGroups = directoryGroup "./templates"


processformNewTask :: ServerPartT IO Response
processformNewTask = do
  TaskDescription desc <- getData'
  if null (B.unpack desc)
     then 
         return.toResponse$"error, blank new task"
     else do
       tid <- query $ AskTaskTracker
       let task = (Task tid desc False)
       res <- update $ AddTask task
       if res 
        then do            
            seeOther "/tasks/todo" (toResponse "")          
        else do
            return (toResponse "Ooops Something went wrong")

tickTask :: ServerPartT IO Response
tickTask = do
  TIFF tid <- getData'
  update$ SetTaskStatus tid True
--  return $ toResponse (show tid)
  seeOther "/tasks/todo" (toResponse "")

untickTask :: ServerPartT IO Response
untickTask = do
  TIFF tid <- getData'
 -- return $ toResponse (show tid)
  update $ SetTaskStatus tid False
  seeOther "/tasks/done" (toResponse "")

removeTask :: ServerPartT IO Response
removeTask = do
  TIFF tid <- getData'
  LastURL url <- getData'
  update $ DeleteTask tid
  seeOther url (toResponse "")