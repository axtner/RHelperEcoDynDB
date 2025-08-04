#' create new project keywords for the EcoDyn database
#' 
#' createProjKeywords 
#' 
#' Function to create new project keywords for the EcoDyn database.
#' 
#' @export

createProjKeywords = function(proj_name){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  
  # query existing projects
  if(exists("projects", envir = .GlobalEnv) == T){
    projects <<- get0("projects", envir = .GlobalEnv)
  } else {
    projects <<- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
  }
  
  # set project name and ID ----
  if((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", envir = .GlobalEnv) == T)){
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
    type = "new project"
    } else {
      writeLines("\nWelcome, you want to link or enter keywords to an existing project.\nStep 1: Select project:")
      project <<- utils::select.list(paste(projects$proj_name, projects$proj_year), 
                                     title = "\nSelect project by choosing it's \naccording number or '0' for exiting:", graphics = FALSE)
    project <<- projects[paste(projects$proj_name, projects$proj_year) == project,]
    proj_name = project$proj_name
    proj_id = project$proj_id
    proj_year = project$proj_year
    if(length(project) == 1){
      rm(project)
      stop("\nYou decided to exit. Bye!")}
    }
  
  
  writeLines("\n\nStep 2: Select keywords:") 
  # query existing keywords from EcoDyn database 
  keywords_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
  
  # check for existing keywords of this project and exclude all previously entered for this project
  keywords_db_proj = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_keywords"))
  keywords_db_proj = keywords_db_proj$keyword[keywords_db_proj$proj_id == proj_id]
  if(length(keywords_db_proj) != 0){
    writeLines(paste0("\nFor the project '", proj_name, ", ", proj_year, "' the following keywords already exist:"))
    writeLines(paste0(sort(keywords_db_proj), collapse = "', '"))
  }
  
  # select project keywords
  proj_keywords = utils::select.list(c(sort(keywords_db$keyword), "other"), graphics = F, multiple = T)

  # when 'other' was chosen...
  other_keys = NA
  if(any(proj_keywords == "other")){
    writeLines("\nStep 2.1: You want to enter new keywords:")
    other_keys = readline("Enter new keywords seperated by ';':")
    other_keys = gsub("; ", ";", other_keys)
    other_keys = strsplit(other_keys, ";")[[1]]
    proj_keywords = proj_keywords[proj_keywords != "other"]
    
    # case one, new keywords were separated by comma
    if(length(other_keys)!=0){
      while (any(grepl("\\,", other_keys) == T)) {
        message("Keywords should not contain ','. Please seperate them by ';'!")
        other_keys = readline("Enter new keywords seperated by ';':")
        other_keys = gsub("; ", ";", other_keys)
        other_keys = strsplit(other_keys, ";")[[1]]
        proj_keywords = proj_keywords[proj_keywords != "other"]
      }
    }
    
    # case two, 'other' was selected but now new keyword entered
    if(other_keys == ""){
      while(other_keys == ""){
        message("When the option 'other' was chosen you must define new keywords or deselect 'other'.")
        rechoice = utils::select.list(c("Deselect other", "Define new keywords"), 
                                      title = "Please chose by typing '1' or '2':", graphics=F)
        
        if(rechoice == "Define new keywords"){
          other_keys = readline("Enter new keywords seperated by ';':")
          other_keys = gsub("; ", ";", other_keys)
          other_keys = strsplit(other_keys, ";")[[1]]
          proj_keywords = proj_keywords[proj_keywords != "other"]
          
          while (any(grepl("\\,", other_keys) == T)) {
            message("Keywords should not contain ','. Please seperate them by ';'!")
            other_keys = readline("Enter new keywords seperated by ';':")
            other_keys = gsub("; ", ";", other_keys)
            other_keys = strsplit(other_keys, ";")[[1]]
            proj_keywords = proj_keywords[proj_keywords != "other"]
          }
        } else {
          proj_keywords = proj_keywords[proj_keywords != "other"]
          message("\nYou'll continue without adding new keywords...")
          rm(other_keys)
        }
      } 
    } 
  }
  
  if(exists("other_keys") == T){
    DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", table = "keywords"), data.frame(keyword = other_keys), append = T)
    proj_keywords = c(proj_keywords, other_keys)
  } else {
    proj_keywords = proj_keywords[proj_keywords != "other"]
  }
  
  # check for existing keywords of this project and exclude all previously entered for this project
  keywords_db_proj = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_keywords"))
  keywords_db_proj = keywords_db_proj$keyword[keywords_db_proj$proj_id == proj_id]
  proj_keywords = proj_keywords[!(proj_keywords %in% keywords_db_proj)]
  
  # write entries into the proj_keywords table
  writeLines(paste0("proj_name: ", proj_name))
  writeLines(paste0("proj_id: ", proj_id))
  DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_keywords"), data.frame(proj_id = projects$proj_id[projects$proj_name == proj_name], keyword = proj_keywords), append = T)

  # final message
  writeLines(paste0("\nFor the project '", proj_name, ", ", proj_year, "' the following keywords were added:"))
  writeLines(paste0("'",proj_keywords,"'", collapse = ", "))
  if(type == "new project"){
    rm(list = ls())
  }
  
}
