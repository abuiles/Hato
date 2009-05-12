<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head>
    <title>To-Do</title>
  </head>
  <body>
    <h1>To-Do List</h1>
    <form name="selectStatus">
     What do you want to see ?
<select name="status"
 OnChange="location.href=selectStatus.status.options[selectedIndex].value">  
     <option selected>Please Select...
     <option value="/tasks/todo/">Todo Tasks
     <option value="/tasks/done">Done Tasks     
</select> 
</form>
   
    <div id="content">
    	 $ contentarea $
    </div>
     <br/>
    <form method="post" id="newTaskForm" action="/tasks/newTask" >
        New Task  : </br>
	<input type="text" name="newTaskField"/> 
	<input type="submit" value="Add" />
     </form>     
  </body>
</html>
