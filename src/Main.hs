-- Add edit task
module Main where

import Happstack.Server
import Happstack.State
import Happstack.Helpers
import Controller
import StateVersions.AppState

main :: IO ()
main = do
  stgrs <- templatesGroups--getTemplateGroups
  smartserver (Conf 8080 Nothing) "Todo" (tasksController stgrs) stateProxy
  


stateProxy :: Proxy AppState
stateProxy = Proxy