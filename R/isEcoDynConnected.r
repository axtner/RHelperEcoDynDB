#' test for connection to EcoDyn database
#' 
#' isEcoDynConnected 
#' 
#' Tests if an connection to the EcoDyn database exists. Deploys the DBI function dbIsValid().
#' 
#' @export

isEcoDynConnected <- function() {
  if (exists("db_con") && !is.null(db_con)) {
    db_con <- get("db_con", envir = .GlobalEnv)
    return(DBI::dbIsValid(db_con))
  } else {
    return(FALSE)
  }
}
