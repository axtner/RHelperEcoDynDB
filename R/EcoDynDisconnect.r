#' disconnect to EcoDyn database
#' 
#' EcoDynDisconnect 
#' 
#' Function tests if a connection to the EcoDyn database exists and disconnect from it. Deploys the DBI function dbDisconnect().
#' 
#' @export

EcoDynDisconnect = function(){
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
    DBI::dbDisconnect(db_con)
    rm("db_con", envir = .GlobalEnv)
    cat("Disconnected from EcoDyn database\n")
  } else {
    message("No database connection exists.")
  }
}
