#' create new project in the EcoDyn database
#' 
#' createProject 
#' 
#' Function to create a new project in the EcoDyn database.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

createProject = function(db_user = NA){
 
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
 
  
  keywords <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
  projects <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
  
  
  # project details
  writeLines("\nWelcome! You want to enter a new project to the EcoDyn database.\nIn the first step you will enter the project name and year.")
  writeLines("\n\nStep 1: Enter project name, year adn short description:")
  proj_name <<- readline("Please enter the project name: ")
  while(proj_name == ""){
    message("You have to enter a project name!")
    proj_name <<- readline("Please enter the project name: ")
  }
  proj_year <<- readline("Please enter the year of start: ")
  while(proj_name == ""){
    message("You have to enter a project year!")
    proj_year <<- readline("Please enter the year of start: ")
  }
  proj_description <<- readline("Enter a short project description (max 500 characters): ")
  while(nchar(proj_description) > 500){
    message("Your decription has more than 500 characters!")
    proj_description <<- readline("Enter a short project description (max 500 characters): ")
  }

  # check if project name already exists
  if(tolower(proj_name) %in% tolower(projects$proj_name) == T){
    while(tolower(proj_name) %in% tolower(projects$proj_name) == T){
      message(paste0("A project with name '", proj_name, "' already exists. \nYou can use RHelperDB::db_projects_info(proj_name = \"", proj_name, "\") to see more information on this existing project.\nTo enter data to an existing project use the respective functions \n(*)createProjKeywords() \n(*)createProjPeople() \n(*)createProjOrg()\n\nPlease chose a different project name or exit."))
      choice_1 = utils::select.list(c("Chose new project name", "Exit"), title = "Please chose by typing '1' or '2':")
      if(choice_1 == "Chose new project name"){
        proj_name <<- readline("Project name: ")
      } else {
        stop("\nYou decided to stop the process. Bye!")
      }
    }
  }
 # write new project to DB
  DBI::dbWriteTable(db_con, DBI::Id(schema = "projects", table = "proj_info"), data.frame(cbind(proj_name, proj_year, proj_description)), append = T)
  # renew projects to get the new project ID
  projects <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
  proj_id <<- projects$proj_id[projects$proj_name == proj_name]
  
  writeLines(paste0("\nProject '", proj_name, ", ", proj_year, "' was added to the database.\nYou can now add \n(*) project keywords, \n(*) people, their affiliation and project role, \n(*) organisations related to this project and their role or \n(*) exit the process.\nAll those information you can also add later by calling the respective RHelperEcoDynDB functions (i.e. createProjKeywords(), createProjPeople, createProjOrg())"))
  continue = utils::select.list(c("Add project keywords", "Add project people", "Add project organisations", "Exit"), title = "Please chose how to continue:", multiple = T, graphics=F)
  
  # calling the respective functions defined in 'continue'
  if("Add project keywords" %in% continue){
    createProjKeywords(proj_name = get0("proj_name", envir = .GlobalEnv))
    continue = continue[!(grepl("Add project keywords", continue))]
  } else {
    message("\nYou skipped the step 'keywords'.")
  }
  
  if("Add project people" %in% continue){
    createProjPeople(proj_name = get0("proj_name", envir = .GlobalEnv))
    continue = continue[!(grepl("Add project people", continue))]
  } else {
    message("\nYou skipped the step 'people'.")
  }
  
  if("Add project organisations" %in% continue){
    createProjOrg(proj_name = get0("proj_name", envir = .GlobalEnv))
    continue = continue[!(grepl("Add project organisations", continue))]
  } else {
    message("\nYou skipped the step 'organisations'.")
  }
  
  if("Exit" %in% continue | length(continue == 0)){
    message("You decided to leave... Have a nice day!")
    stop(sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" ")))
  }
  
  # final message
  writeLines(paste0("\nYou entered all relevant data. \nYou can add additional project information any time by calling the respective functions\ncreateProjKeywords(proj_name = ", proj_name,"), \ncreateProjPeople(proj_name = ", proj_name,"), \ncreateProjOrg(proj_name = ", proj_name,"))\n\nHave a nice day!"))
  rm(proj_id, proj_name)
}
