#' create paper in the EcoDyn database
#' 
#' createPaper
#' 
#' Function to add a paper to the EcoDyn database.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

createPaper = function(db_user = NA){
 
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
 
  doi <- base::readline("doi:")
  
    # helper to extract value for a key (handles {...} or "...")
    get_field <- function(key, text) {
      pat <- paste0(key, "\\s*=\\s*(\\{([^}]*)\\}|\"([^\"]*)\"|([^,\\}]+))")
      m <- regexec(pat, text, perl = TRUE, ignore.case = TRUE)
      r <- regmatches(text, m)[[1]]
      if (length(r) == 0) return(NA_character_)
      # r[3] is inside {}, r[4] inside "", r[5] unbraced
      val <- r[3]
      if (is.na(val) || val == "") val <- r[4]
      if (is.na(val) || val == "") val <- r[5]
      val <- trimws(val)
      # remove trailing commas/spaces
      val <- sub(",\\s*$", "", val)
      # normalize multiple whitespace
      gsub("\\s+", " ", val)
    }
    
    keys <- c("author","title","journal","year","volume","number","pages","doi","url",
              "publisher","month","issn")
    values <- setNames(lapply(keys, get_field, text = rcrossref::cr_cn(doi)), keys)
    # collapse author "and" separators into a single string (keep as-is)
    df <- as.data.frame(values, stringsAsFactors = FALSE)
  
  dbWriteTable(con, 
               name = DBI::Id(schema = "bibdata", table = "bibtable"), 
               df, append = TRUE)
}
