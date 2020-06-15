data_connection <- function(){
  req(db_connection())
    db_connection()
}

data_all<-function(){
  req(data_connection())
    
  fl <- '{"_id":1,
  "projectPassword":1,
  "projectName":1,
  "projectDescription":1,
  "institutionName":1,
  "projectAutor":1,
  "projectDate":1,
  "projectData":1}'

  data_connection()$find(fields=fl)
}
 
data_specific <- function(project_id){
  req(data_connection())
  
  q <- sprintf('{"_id" : {"$oid":"%s"}}', project_id)
  
  data_connection()$find(query = q)
}

data_first<-function(){
  req(data_connection())
  
  data_connection()$find(sort = '{"_id":1}', limit = 1, fields = '{"_id":1}')
}

password__delete <- function(project_id, project_pass,path_data){
  q <- sprintf('{"_id" : {"$oid":"%s"}}', project_id)
  r <- data_connection()$find(query = q) %>% select(projectPassword)
  
  if(r == project_pass){
    project_delete(q,path_data)
    return("Successfully removed")
  }
    return("Incorrect password")
}

project_delete <- function(q,path_data){
  data_connection()$remove(query = q, just_one = TRUE)
  file.remove(sprintf("%s",path_data))
}

password_edit <- function (project_id, project_pass, project_description){
  q <- sprintf('{"_id" : {"$oid":"%s"}}', project_id)
  r <- data_connection()$find(query = q) %>% select(projectPassword)
  
  if(r == project_pass){
    project_edit(q, project_description)
    return("Successfully edited")
  }
  return("Incorrect password")
}

project_edit <- function(q, project_description){
  u <- sprintf('{"$set":{"projectDescription":"%s"}}', project_description)
  data_connection()$update(query = q, update = u)
}

password_value <- function (project_id, project_pass){
  q <- sprintf('{"_id" : {"$oid":"%s"}}', project_id)
  r <- data_connection()$find(query = q) %>% select(projectPassword)
  
  if(r == project_pass){
    return("Successfully edited")
  }
  return("Incorrect password")
}
  


