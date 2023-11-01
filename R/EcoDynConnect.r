#' connect to EcoDyn database
#' 
#' EcoDynConnect 
#' 
#' Function to connect to EcoDyn database. Deploys the DBI function dbConnect().
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' @param db_name Database name. Must not be provided by the user.
#' @param db_host IP address of the server hosting the EcoDyn database. Must not be provided by the user.
#' @param db_port Server port of the EcoDyn database.
#' 
#' @export

EcoDynConnect = function(db_user = NA,
                         db_name = "EcoDynDB",
                         db_host = "192.168.2.83",
                         db_port = "5432"){
 
  # ask for DB user and password
  if(is.na(db_user)){
    db_user <- readline("database user: ")
  }
  
  # db password query
  db_password = getPass::getPass("database password: ")
  
  # establish connection to EcoDynDB
  db_con <<- DBI::dbConnect(RPostgres::Postgres(), 
                           user = db_user,
                           password = db_password,
                           dbname = db_name,        
                           host = db_host,
                           port = db_port)
  # send message
  writeLines(paste0("Connecting to ", db_name, " database.\nTo disconnect from database use 'EcoDynDisconnect()'."))
  }
