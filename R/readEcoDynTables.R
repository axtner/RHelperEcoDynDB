#' created overview of available EcoDyn database schemas and tables
#' 
#' readEcoDynTables 
#' 
#' Creates an object 'db_structure' that shows the available schema and tables of the EcoDyn database.
#' 
#' @export
#' 

readEcoDynTables = function(){
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
    
    db_structure <- DBI::dbGetQuery(db_con, 
                                    "SELECT * FROM information_schema.tables 
                                     WHERE table_schema NOT IN ('pg_catalog', 'information_schema') 
                                     AND table_schema NOT LIKE 'pg_toast%'"
                                    )[, 1:3]
    
    db_structure <- db_structure[order(db_structure$table_schema, db_structure$table_name),]
    row.names(db_structure) <- c(1 : nrow(db_structure))
    colnames(db_structure) <- c("db_name", "schema_name", "table_name")
    db_structure <- db_structure
    writeLines("created R dataframe object 'db_structure':\n")
    print(db_structure)
  } else {
    message("No database connection exists.")
  }
}