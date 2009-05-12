module View where

import Text.StringTemplate.Helpers
import Text.StringTemplate



pageLayout:: (STGroup [Char]) -> [(String,String)]  ->  String
pageLayout stg attrs = renderTemplateGroup stg attrs "base"  

type TaskId = Integer
type TaskStatus = Bool
type CheckBoxCode  = String


checkBox :: TaskStatus -> TaskId ->  CheckBoxCode
checkBox status tid = if status
                       then 
                          "<form action='/tasks/untick' method='post' ><input type=\"hidden\" name='tid' value='"++tidS++"'/><input type=\"checkbox\" OnChange=\"this.form.submit()\" checked='checked'  > </input></form>" 
                        else
                         "<form action='/tasks/tick' method='post' ><input type=\"hidden\" name='tid' value='"++tidS++"'/> <input type=\"checkbox\" OnChange=\"this.form.submit()\" > </input></form>"
    where 
      tidS = show tid

type DeleteButtonCode = String
type CurrentUrl = String

deleteButton :: TaskId -> CurrentUrl ->  DeleteButtonCode
deleteButton tid url = "<form method=\"post\" action=\"/tasks/delete\"> <input type=\"hidden\" name='lasturl' value='"++ url ++"'/> <input type=\"hidden\" name='tid' value='"++show tid++"'/> <input type=\"submit\" value=\"delete\"  </form>"
-- Problem when trying to submit an unselected checkbox...solved! 