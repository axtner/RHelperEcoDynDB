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
                         db_host = NA,
                         db_port = NA){
 
  # ask for DB user and password
  if(is.na(db_user)){
    db_user <- readline("database user: ")
  }
  
  # db password query
  db_password = getPass::getPass("database password: ")

  
  # check if inside IZW domain and if EcoDynserver is available
  if(is.na(db_host) & is.na(db_port) == T){
    if(pingr::is_up("192.168.2.83", port="5432") == F){
    db_host <<- "192.168.2.83"
    db_port <<- "5432"
    } else {
      message("Either you are outside the IZW domain or the EcoDyn server is offline.")
      message("Redirection via 'localhost'. Make sure your ssh port forwarding is correct.")
      db_host <<- "localhost"
      db_port <<- "5433"
      } 
  }
  
  # check for individual db_host and db_port setting
  if(is.na(db_host) | is.na(db_port) == T){
    stop("\nIf you want to use different 'db_host' and 'db_port' settings than default you must define both!")
  }
 
  
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
