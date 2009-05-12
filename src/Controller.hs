module Controller where

import Happstack.Server
import Happstack.Helpers

import Control.Monad -- MonadPlus
import ControllerGetActions
import ControllerPostActions
import Text.StringTemplate

import qualified Data.Map as M

templatesGroups ::IO (STGroup [Char])
templatesGroups = directoryGroup "./templates"

getTemplateGroups :: (Stringable a) => IO (M.Map FilePath (STGroup a))
getTemplateGroups = directoryGroupsHAppS "templates" 


tasksController :: (STGroup [Char]) -> ServerPartT IO Response
tasksController stgr =  dir "tasks" ( msum [ dir "todo"  ( viewTasksByStatus False stgr "/tasks/todo"),
                                             dir "done"  ( viewTasksByStatus True stgr "/tasks/done"),
                                             dir "newTask" ( methodSP POST $ processformNewTask),
                                             dir "tick" ( methodSP POST $ tickTask ),
                                             dir "untick" ( methodSP POST $ untickTask ),
                                             dir "delete" (methodSP POST $ removeTask ),
                                             mzero
                                           ]
                                    )

