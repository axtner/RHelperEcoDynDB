#' helper function to format and run existing SQL scripts from R
#' 
#' getSQL 
#' 
#' Function to format and run SQL scripts from R. Use DBI::dbGetQuery(db_con, getSQL("CHANGEthisFILE.sql"))
#' 
#' @param filepath File path to SQL script.
#' 
#' 
#' @export

getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}

