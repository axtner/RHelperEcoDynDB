#' enter new project organisations to the EcoDyn database
#' 
#' createProjOrg 
#' 
#' Function to add people to the EcoDyn database that participate in projects.
#' 
#' @export

createProjOrg = function(proj_name){
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  
  
  # query existing projects
  if(exists("projects", envir = .GlobalEnv) == T){
    projects <<- get0("projects", envir = .GlobalEnv)
  } else {
    projects <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
  }
  # set project name and ID
  if((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", envir = .GlobalEnv) == T)){
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
    type = "new project"
  } else {
    writeLines("\nWelcome, you want to link or enter organisations to an existing project.\nStep 1: Select project:")
    project <<- utils::select.list(paste(projects$proj_name, projects$proj_year), 
                                   title = "\nSelect project by choosing it's \naccording number or '0' for exiting:", graphics = FALSE)
    project <<- projects[paste(projects$proj_name, projects$proj_year) == project,]
    proj_name = project$proj_name
    proj_id = project$proj_id
    proj_year = project$proj_year
    type = "existing project"
    if(length(project) == 1){
      rm(project)
      stop("You decided to exit. Bye!")
      }
  }
  
  add_org = function(){ 
    # query existing organisations from the DB
    orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
    if(type == "new project"){
      writeLines("\n\nStep 4: Organisation details\nTo link a organisation and role to this project please follow the instructions.")
      } else {
        writeLines("\n\nStep 2: Organisation details\nTo link a organisation and role to this project please follow the instructions.")
      }
    # filter those organisations by part of their name, e.g. "re" or "wil" for "re:wild"
    if(type == "new project"){
      writeLines("\nStep 4.1: Select an organisation")} else {
        writeLines("\nStep 1.1: Select an organisation")
      }
    o_filter = readline("Enter part of organisation name: ")
    f_org = orgs[grepl(tolower(o_filter), tolower(paste(orgs$organisation, orgs$abbreviation, sep=", "))),]
    organisation = utils::select.list(c(sort(paste(f_org$organisation, f_org$abbreviation, sep = ", ")), "other"), graphics=F)
    writeLines(paste0("Chosen organsiation: ", organisation))
    # enter new organisation
    if(type == "new project"){
      writeLines("\nStep 4.1.1: Enter new organisation")} else {
        writeLines("\nStep 1.1.1: Enter new organisation")
      }
    if(organisation == "other"){
      writeLines("\nYou want to add a unknown organisation to the database, please fill out the following:")
      org <- readline("1. Full name of Organisation: ")
      abbr <- readline("2. Abbreviation of Organisation: ")
      countries = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "countries"))
      # filter those people by part of their country name, e.g. "ge" for "Germany"
      c_filter = readline("To select the country the organisaton is resident, enter part of the country name: ")
      f_country = countries[grepl(tolower(c_filter), tolower(countries$country)),]
      country = utils::select.list(f_country, graphics=F)
      address = readline("Please enter full address: ")
      # check if entered organisation does not exist in database
      if(org %in% orgs$organisation){
        stop(paste0("'", org,"' already exists in the database. Please check!"))
      } else {
        # write new organisation to database
        DBI::dbWriteTable(db_con, DBI::Id(schema="people", table="organisations"), data.frame(org, abbr, address, country), append = T )
      }
    }
    
    # organisaion details
    if(organisation != "other"){
      org = strsplit(organisation, ",")[[1]][1]
    }
    
    # new query of organisations and their ID from the database
    orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
    org_id = orgs$org_id[orgs$organisation == org]
    
    # organisation's role in the project----
    if(type == "new project"){
      writeLines("\nStep 4.2: Organisation's role")} else {
        writeLines("\nStep 1.2: Organisation's role")
      }
    add_role = function(){
      # query existing organisation roles from the database
      roles <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_org_roles"))
      writeLines("\nPlease enter the organisation's project role(s).")
      proj_role = utils::select.list(c(sort(roles$proj_org_role), "other"), graphics=F)
      
      # enter new role when 'other' was chosen...
      if(proj_role == "other"){
        if(type == "new project"){
          writeLines("\nStep 4.2.1: New role")
        } else {
          writeLines("\nStep 1.2.1: New role")
        }
        writeLines("\nYou could not find an appropiate role and want to add a new role.")
        proj_role = readline("Enter new role title: ")
        
        # check if proj_role is empty
        if(proj_role == ""){
          while(proj_role == ""){
            message("When the option 'other' was chosen you must define new role or deselect 'other'.")
            rechoice = utils::select.list(c("Deselect other", "Define new role"), 
                                          title = "Please chose by typing '1' or '2':", graphics=F)
            if(rechoice == "Define new role"){
              proj_role = readline("Enter new role title: ")
            } else {
              proj_role = utils::select.list(sort(roles$proj_org_role), graphics=F)
            }
          }
        }
        
        # write new role to database
        if((proj_role %in% roles) == F){
          DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_org_roles"), data.frame("proj_org_role" = proj_role), append = T)
        }
      }
      
      # write proj_id, org_id and proj_role to the projects.proj_organisations table of the database
      DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_organisations"), data.frame(proj_id, org_id, "proj_org_role"= proj_role), append = T)
      writeLines(paste0("\n'", org, "' was added to project '", proj_name, " ", proj_year, "' as ",proj_role,"."))
      writeLines("\nDoes the organisation have a second role in this project?")
      sec_role <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", graphics=F)
    }
    
    # run function add_role() to link a project role to a organisation
    add_role()
    # re-run function if a additional roles are relevant
    while(sec_role == "YES"){
      add_role()
    } 
    if(sec_role == "NO"){
      # final role message
      writeLines(paste0("\nYou entered all relevant roles of organisation '", org, "' and linked it to the project '", proj_name, "'.\nYou can always add additional roles by running the createProjOrg() function."))
    }
    # ask if another organisation should be added 
    writeLines("\nDo you want to enter another relevant organisation to this project?")
    sec_org <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", graphics=F)
    } # end of add_org()
  
  # run the select_person() function
  add_org()
  
  # re-run the select_person() function to add more persons
  while(sec_org == "YES"){
    add_org()
  } 
  if(sec_org == "NO"){
    # final message
    writeLines(paste0("\nYou entered all relevant organisations to the project '", proj_name, "'.\nYou can always add additional organisations, by running the createProjOrg() function."))
  }  
  
  
}# end of createProjOrg()
    
    
    
  