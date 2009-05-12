{-# LANGUAGE TemplateHaskell,TypeFamilies, DeriveDataTypeable, MultiParamTypeClasses,FlexibleInstances,FlexibleContexts,TypeSynonymInstances #-}
module StateVersions.AppState{-(Task(..),SetTaskStatus, SetTaskDesc, AskTask, AddTask, AskTasks (..), start) -}  where

import Happstack.State
import qualified Data.ByteString.Char8 as B
import Data.Generics
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map as M


newtype TaskTracker = TaskTracker { unTaskTracker :: Integer }
    deriving (Show,Eq,Read,Typeable,Data)

instance Version TaskTracker
$(deriveSerialize ''TaskTracker)

data Task = Task { tId :: Integer,
                   tDescr :: B.ByteString , --Task description
                   tStatus :: Bool
                                      -- true = done , false = todo   
                 }
          deriving (Show,Eq,Read,Typeable,Data)



instance Version Task
$(deriveSerialize ''Task)

-- a -> Task ->Task
set_taskStatus :: Bool -> Task -> Task
set_taskStatus s t = t {tStatus = s}

set_taskDescr :: B.ByteString -> Task -> Task
set_taskDescr d t = t {tDescr = d}


type TaskId = Integer

data AppState = AppState { appdatastore :: (M.Map TaskId Task)  , taskTracker :: TaskTracker}
              deriving (Show,Read,Typeable,Data)


instance Version AppState 
$(deriveSerialize ''AppState)

instance Component AppState where
    type Dependencies AppState = End
    initialValue = AppState { appdatastore =M.empty , taskTracker = (TaskTracker 0) }

askTaskTracker :: Query AppState Integer
askTaskTracker =  fmap (\s@(AppState _ tt) -> unTaskTracker tt) ask

incrTaskTracker :: Update AppState ()
incrTaskTracker = modify (\s@(AppState _ tt) -> s {taskTracker = TaskTracker ((unTaskTracker tt) +1)})

askTasks :: Query AppState [Task]
askTasks = fmap (M.elems.appdatastore) ask 

askTasksByStatus :: Bool -> Query AppState [Task]
askTasksByStatus v = fmap ((M.elems.M.filter (\t -> (tStatus t) == v)).appdatastore) ask 

addTask ::Task ->  Update AppState Bool
addTask t =do modify (\s@(AppState ds _) -> s{ appdatastore = (M.insert (tId t) t ds) })
              incrTaskTracker
              return True

askTask :: Integer -> Query AppState (Maybe Task)
askTask tid = fmap ((M.lookup tid).appdatastore) ask

{- There are some common patterns when modifying a task field.
   This function capture those common patterns, so that It just received
   as parameters the task's id which we want to modify , and a mutator function which takes a Task and return a Task,
   the final result is a Bool, indicating whether the operation was done or not. True = done , False = fail -}

setTaskField :: Integer  -> (Task -> Task )-> Update AppState ()
setTaskField tid f = do
  s@(AppState old _) <- get
  let xs = M.adjust f tid old 
  put (s { appdatastore = xs })
  

setTaskStatus ::Integer -> Bool ->  Update AppState ()
setTaskStatus tid ns = setTaskField tid (set_taskStatus ns)


setTaskDesc :: Integer -> B.ByteString -> Update AppState ()
setTaskDesc tid nd = setTaskField tid (set_taskDescr nd)

deleteTask :: Integer -> Update AppState ()
deleteTask tid = modify (\s@(AppState ds _) -> s{ appdatastore = (M.delete tid ds) })
            
  
                      
 
--Tests helper
newTask ::  String -> Bool -> Query AppState Task
newTask a b = do 
  ctt <- askTaskTracker 
  return ( Task ctt  (B.pack a) b)


showTa :: Query AppState String
showTa = fmap (M.showTree.appdatastore) ask 

$(mkMethods ''AppState ['showTa,'addTask,'askTask,'askTaskTracker,'askTasks,'askTasksByStatus,'deleteTask,'incrTaskTracker,'newTask ,'setTaskDesc,'setTaskStatus ])                

