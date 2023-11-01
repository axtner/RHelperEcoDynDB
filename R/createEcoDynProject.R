#' create new project in the EcoDyn database
#' 
#' newEcoDynProject 
#' 
#' Function to create a new project in the EcoDyn database.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

newEcoDynProject = function(db_user = NA){
 
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
 
  
  keywords <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
  projects <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
  
  
  # project details
  proj_name <- readline("Project name: ")
  proj_year <- readline("Project start year: ")

  # check if project name already exists
  if(tolower(proj_name) %in% tolower(projects$proj_name) == T){
    while(tolower(proj_name) %in% tolower(projects$proj_name) == T){
      message(paste0("A project with name '", proj_name, "' already exists. Please check or chose a different name.\nYou can use RHelperDB::db_projects_info(proj_name = \"", proj_name, "\") to see more information on this project.\nPlease chose if you want to change project name an continue or if you want to stop."))
      choice_1 = utils::select.list(c("Chose new project name", "Exit"), title = "Please chose by typing '1' or '2':")
      if(choice_1 == "Chose new project name"){
        proj_name <- readline("Project name: ")
      } else {
        stop("You decided to stop the process.")
      }
    }
  }
  # write new projec to DB
  DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", table = "proj_info"), data.frame(cbind(proj_name, proj_year)), append = T)
  # renew projects to get the new project ID
  projects <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
  proj_id <- projects$proj_id[projects$proj_name == proj_name]
  
  writeLines(paste0("Project ", proj_name, " was added to the database.\nYou can add now project keywords, skip to the next step or exit the process.\nYou can also add project keywords later by using RHelperDB::db_proj_keywords(proj_name = \"", proj_name, "\")"))
  step2 = utils::select.list(c("Chose project keywords", "Skip keywords, next...", "Exit"), title = "Please chose by typing '1', '2' or '3':")
  if(step2 == "Chose project keywords"){
    #db_proj_keywords(proj_name = get0("proj_name", envir = .GlobalEnv))
    createProjKeywords()
  }  
  if(step2 == "Skip keywords, next..."){
    cat("Chose project people...")
  } 
  if(step2 == "Exit"){
    writeLines("You leave... Have a nice day!")
    stop(sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" ")))
  }
}
