#' create new project keywords for the EcoDyn database
#' 
#' createProjKeywords 
#' 
#' Function to create new project keywords for the EcoDyn database.
#' 
#' @export

createProjKeywords = function(proj_name){
  
  if(exists("proj_id") == F){
    message("Welcome, you want to link or enter keywords to one or more existing projects.\nStep 1: Select project(s):")
    RHelperEcoDynDB::selectProject()
  }
  
  if(is.na(proj_id)){
    message("No project ID available, please select project.")
    RHelperEcoDynDB::selectProject()
  }
  
  if(exists("proj_id") == T){
    type <<- "new project"
    } 
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  message("\n\nStep 2: Select keywords:") 
  if(length(proj_id) > 1){
    message("Selected keywords will be applied to all selected projects.")
  }
  # query existing keywords from EcoDyn database 
  keywords_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
  
  # check for existing keywords of this project and exclude all previously entered for this project
  #keywords_db_proj = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_keywords"))
  #keywords_db_proj = keywords_db_proj$keyword[keywords_db_proj$proj_id == proj_id]
  
  for(i in 1 : length(proj_id)){
    q_i = paste0("'", proj_id[i], "'")
    if(i == 1){
      q_1 = q_i
    }
    if(i > 1){
      q_1 = paste(q_1, q_i, sep = ", ")
    }
  }
  existing_keywords = DBI::dbGetQuery(db_con, paste0("SELECT pk.proj_id, pk.keyword_id, k.keyword FROM projects.proj_keywords pk LEFT JOIN projects.keywords k ON k.keyword_id = pk.keyword_id WHERE pk.proj_id IN (", q_1,")"))
  
  if(nrow(existing_keywords) != 0){
    projects_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
    projects = projects_db[projects_db$proj_id %in% proj_id,]
    for(i in 1 : nrow(projects)){
      writeLines(paste0("\nFor the project '", projects$proj_name[i], "' the following keywords already exist:"))
      writeLines(paste0("'",paste0(sort(existing_keywords$keyword[existing_keywords$proj_id == projects$proj_id[i]]), collapse = "', '"), "'"))
    }
  }
  
  
  # select project keywords
  proj_keywords <<- utils::select.list(c(sort(keywords_db$keyword), "other"), graphics = F, multiple = T)

  # when 'other' was chosen...
  if(any(proj_keywords == "other")){
    writeLines("\nStep 2.1: You want to enter new keywords:")
    other_keys <<- readline("Enter new keywords seperated by ';':")
    other_keys <<- gsub("; ", ";", other_keys)
    other_keys <<- strsplit(other_keys, ";")[[1]]
    proj_keywords <<- proj_keywords[proj_keywords != "other"]
    
    # case one, new keywords were separated by comma
    if(length(other_keys)!=0){
      while (any(grepl("\\,", other_keys) == T)) {
        message("Keywords should not contain ','. Please seperate them by ';'!")
        other_keys <<- readline("Enter new keywords seperated by ';':")
        other_keys <<- gsub("; ", ";", other_keys)
        other_keys <<- strsplit(other_keys, ";")[[1]]
        proj_keywords <<- proj_keywords[proj_keywords != "other"]
      }
    }
    
    # case two, 'other' was selected but now new keyword entered
    if(other_keys == ""){
      while(other_keys == ""){
        message("When the option 'other' was chosen you must define new keywords or deselect 'other'.")
        rechoice = utils::select.list(c("Deselect other", "Define new keywords"), 
                                      title = "Please chose by typing '1' or '2':", graphics=F)
        
        if(rechoice == "Define new keywords"){
          other_keys <<- readline("Enter new keywords seperated by ';':")
          other_keys <<- gsub("; ", ";", other_keys)
          other_keys <<- strsplit(other_keys, ";")[[1]]
          proj_keywords <<- proj_keywords[proj_keywords != "other"]
          
          while (any(grepl("\\,", other_keys) == T)) {
            message("Keywords should not contain ','. Please seperate them by ';'!")
            other_keys <<- readline("Enter new keywords seperated by ';':")
            other_keys <<- gsub("; ", ";", other_keys)
            other_keys <<- strsplit(other_keys, ";")[[1]]
            proj_keywords <<- proj_keywords[proj_keywords != "other"]
          }
        } else {
          proj_keywords <<- proj_keywords[proj_keywords != "other"]
          message("\nYou'll continue without adding new keywords...")
          rm(other_keys)
        }
      } 
    } 
  }
  
  if(exists("other_keys") == T){
    DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", table = "keywords"), data.frame(keyword = other_keys), append = T)
    proj_keywords <<- c(proj_keywords, other_keys)
  } else {
    proj_keywords <<- proj_keywords[proj_keywords != "other"]
    writeLines("Chosen key words:")
    print(proj_keywords)
  }
  
  # get keyword ids from database
  keywords_db <-DBI::dbGetQuery(db_con, paste0("SELECT * FROM projects.keywords;"))
  
  # exclude all previously entered keywords for this project
  if(nrow(existing_keywords) >0){
    proj_keywords <<- proj_keywords[!(proj_keywords %in% existing_keywords$keyword)]
  }
  
  # adding keyword id
  proj_keywords <<- keywords_db[keywords_db$keyword %in% proj_keywords,]
    
    
  if(length(proj_keywords) > 0){
    # write entries into the proj_keywords table
    new_data = expand.grid(proj_id = proj_id, keyword_id = proj_keywords$keyword_id)
    DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_keywords"), new_data, append = T)
  }

    # final message
    if(nrow(proj_keywords) > 1){
      message(paste0(paste0("'",proj_keywords$keyword,"'", collapse = ", "), " were added as keywords to the projects ",paste0("'",proj_name,"'", collapse = ", "), "."))
    }
    if(nrow(proj_keywords) == 1){
      message(paste0(paste0("'",proj_keywords$keyword,"'", collapse = ", "), " was added as keyword to the projects ",paste0("'",proj_name,"'", collapse = ", "), "."))
    } else {
      message(paste0("No new keywords were added. All chosen keywords do already exist for the projects ", paste0("'",proj_name,"'", collapse = ", "), "."))
      }
  if(exists("type") == F){
    rm(list = ls())
  }
}
