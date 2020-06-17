################################################################################
# Module data base connection
#
# Author: Cristian Pazmiño
# Created: 2020-04-09 15:25:10
################################################################################

db_connection <- reactive({
  req(db_credentials())
  
  tryCatch({
    
    mongo_db_user <- db_credentials()[1,1]
    mongo_db_password <- db_credentials()[1,2]
    mongo_db_database <- toString(db_credentials()[1,3])
    mongo_db_collection <- toString(db_credentials()[1,4])
    mongo_db_cluster <- db_credentials()[1,5]
    
    mongo_db_url <- sprintf("mongodb+srv://%s:%s@%s", mongo_db_user, mongo_db_password, mongo_db_cluster)
    
    connection <- mongo(collection = mongo_db_collection, db = mongo_db_database, url = mongo_db_url, verbose = FALSE)

    cat("Successful connection \n")
    
    return(connection)
  }, error = function(e) {
    
    cat("Failed connection \n")
    print(e)
  })
})

