#' create new project keywords for the EcoDyn database
#' 
#' createProjKeywords 
#' 
#' Function to create new project keywords for the EcoDyn database.
#' 
#' @export

createProjKeywords = function(){
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  
  if(exists("proj_name", envir = .GlobalEnv) == F){
    stop("No project selected")
  } else {
    proj_name <- get("proj_name", envir = .GlobalEnv)
  }
  if(exists("proj_id", envir = .GlobalEnv) == T){
    proj_id <- get("proj_id", envir = .GlobalEnv)
  }
  if(exists("projects", envir = .GlobalEnv) == T){
    projects <- get("projects", envir = .GlobalEnv)
  }
  
  # query existing keywords from EcoDyn database
  keywords = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
  
  # select project keywords
  proj_keywords = utils::select.list(c(sort(keywords$keyword), "other"), graphics = T, multiple = T)

  # when 'other' was chosen...
  other_keys = NA
  if(any(proj_keywords == "other")){
    other_keys = readline("Enter new keywords seperated by ';':")
    other_keys = gsub("; ", ";", other_keys)
    other_keys <<- strsplit(other_keys, ";")[[1]]
    if(length(other_keys)!=0){
      while (any(grepl("\\,", other_keys) == T)) {
        message("Keywords should not contain ','. Please seperate them by ';'!")
        other_keys = readline("Enter new keywords seperated by ';':")
        other_keys = gsub("; ", ";", other_keys)
        other_keys <<- strsplit(other_keys, ";")[[1]]
        keywords <<- proj_keywords[keywords != "other"]
      }
    }
    
    if(length(other_keys) == 0){
      while(length(other_keys) == 0){
        message("When the option 'other' was chosen you must define new keywords or deselect 'other'.")
        rechoice = utils::select.list(c("Deselect other", "Define new keywords"), title = "Please chose by typing '1' or '2':")
        
        if(rechoice == "Define new keywords"){
          other_keys = readline("Enter new keywords seperated by ';':")
          other_keys = gsub("; ", ";", other_keys)
          other_keys <<- strsplit(other_keys, ";")[[1]]
          keywords <<- keywords[keywords != "other"]
          
          while (any(grepl("\\,", other_keys) == T)) {
            message("Keywords should not contain ','. Please seperate them by ';'!")
            other_keys = readline("Enter new keywords seperated by ';':")
            other_keys = gsub("; ", ";", other_keys)
            other_keys <<- strsplit(other_keys, ";")[[1]]
            keywords <<- keywords[keywords != "other"]
          }
        } else {
          keywords <<- unique(keywords[keywords != "other"])
          other_keys = NA
        }
      } 
    } 
  }
  if(is.na(other_keys) == F){
    DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="keywords"), data.frame(keyword = other_keys), append = T)
    keywords <<- c(keywords[keywords != "other"], other_keys)
  } else {
    keywords <<- keywords[keywords != "other"]
  }
  
  proj_keywords = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_keywords"))
  proj_keywords = proj_keywords$keyword[proj_keywords$proj_id == proj_id]
  
  keywords = keywords[!(keywords %in% proj_keywords)]
  
  DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_keywords"), data.frame(proj_id = projects$proj_id[projects$proj_name == proj_name], keyword = keywords), append = T)

  writeLines(paste0("Keywords '", paste0(keywords, collapse = "', '"), "' were added to project '", proj_name, "' in the database.\n"))
}
