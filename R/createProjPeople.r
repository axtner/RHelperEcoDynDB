#' enter new project participants to the EcoDyn database
#' 
#' createProjPeople 
#' 
#' Function to add people to the EcoDyn database that participate in projects.
#' 
#' @export

createProjPeople = function(proj_name){
  
  if(exists("proj_id") == F){
    message("Welcome, you want to link or enter persons to one or more existing projects.\nStep 1: Select project(s):")
    RHelperEcoDynDB:::.selectProject()
  }
  
  if(is.na(proj_id)){
    message("No project ID available, please select project.")
    RHelperEcoDynDB:::.selectProject()
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
  
  
  add_person = function(){
    # query existing people from the DB
    people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
    if(type == "new project"){
      message("Step 3.1: Select person:")
      } else {
      message("Step 2.1: Select person:")
      }
    if(length(proj_id) > 1){
      message("Selected person will be linked to all selected projects.")
    }
  # filter those people by part of their family name, e.g. "ax" for "Axtner" or "A" for c("Adam", "Axtner")
  message("In order to restrict names search you can filter for parts or first letters of family names  (e.g. 'ax' for 'Axtner'.\nIf you don want to restrict names search at all just press enter.")
  p_filter = readline("Enter part of family name: ")
  f_people = people[grepl(tolower(p_filter), tolower(paste(people$family_name, people$first_name, sep=", "))),]
  person = utils::select.list(c(sort(paste(f_people$family_name, f_people$first_name, sep = ", ")), "other"), graphics = F)
  # add new person not in database list
  if(person == "other"){
    if(type == "new project"){
      message("\nStep 3.1.1: New person")} else {
        message("\nStep 2.1.1: New person")
      }
    writeLines("\nYou want to add a new person to the database, please fill out the following:")
    family_name <- readline("1. Enter family name: ")
    first_name <- readline("2. Enter first name: ")
    # check if entered person does not already exist in database, else write it to database
    people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
    p_check = paste(people$family_name, people$first_name, sep = ", ")
    if(paste(family_name, first_name, sep = ", ") %in% p_check){
      stop(paste0("'", paste(family_name, first_name, sep = ", "),"' already exists in the database. Please check!"))
    } else {
      DBI::dbWriteTable(db_con, DBI::Id(schema="people", table="people"), data.frame(first_name, family_name), append = T)
      people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
    }
  }
  # person details
  if(person != "other"){
    family_name = strsplit(person, ", ")[[1]][1]
    first_name = strsplit(person, ", ")[[1]][2]
  }
  people_id = people$people_id[paste(people$family_name, people$first_name, sep = ", ") == paste(family_name, first_name, sep = ", ")]
  person = paste(family_name, first_name, sep = ", ")
  
  
  
  # person's affiliation----
  if(type == "new project"){
    message("\nStep 3.2: Person's affiliation")} else {
      message("\nStep 2.2: Person's affiliation")
    }
  add_aff = function(){
    # query existing organisations and affiliations from the database
    orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
    affs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "affiliation"))
    message("\nPlease enter the person's affiliation.\nYou filter the list of organisations for parts or first letters of their names (e.g. 'wildlife' or 'izw' for 'Leibniz Institute of Zoo and Wildlife Research'.\nIf you don want to restrict search just press enter.")
    # filter those organisations by part of their name, e.g. "re" or "wil" for "re:wild"
    o_filter = readline("Enter part of organisation name: ")
    f_org = orgs[grepl(tolower(o_filter), tolower(paste(orgs$organisation, orgs$abbreviation, sep=", "))),]
    organisation = utils::select.list(c(sort(paste(f_org$organisation, f_org$abbreviation, sep = ", ")), "other"))
    
    # enter new organisation
    if(organisation == "other"){
      message("\nYou want to add a unknown organisation to the database, please fill out the following:")
      org = readline("1. Full name of Organisation: ")
      abbr = readline("2. Abbreviation of Organisation: ")
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
        DBI::dbWriteTable(db_con, DBI::Id(schema="people", table="organisations"), data.frame(organisation = org, abbreviation = abbr, address, country), append = T )
      }
    }
    # organisaion details
    if(organisation != "other"){
      org = strsplit(organisation, ",")[[1]][1]
    }
    year_of_aff = readline("Enter relevant year of affiliation: ")
    # re-new query of organisations to get org_id of new entered organisations
    orgs = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
    org_id = orgs$org_id[orgs$organisation == org]
    # check if this affiliation already exists in database
    if((paste(people_id, org_id, year_of_aff) %in% paste(affs$people_id, affs$org_id, affs$year_of_aff)) == F){
      DBI::dbWriteTable(db_con, DBI::Id(schema="people", table="affiliation"), data.frame(people_id, org_id, year_of_aff), append = T)
     message(paste0("\n'", org, "' was added as affiliation for '", year_of_aff, "' for '", person,"."))
    } else {
      message(paste0("\nThe affiliation '", org, " ", year_of_aff, "' for '", person," already exists in the database."))
    }
    message("\nDoes the person have an additional affiliation relevant for this project?")
    sec_aff <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", graphics=F)
  }
  # run function add_aff() to add an affiliation of a person
  add_aff()
  # re-run function if a additional affiliations are relevant
  while(sec_aff == "YES"){
    add_aff()
    }
  if(sec_aff == "NO"){
    # final affiliation message
    message(paste0("\nYou entered all relevant affiliations of person '", person, "' and linked it to the project '", proj_name, "'.\nYou can always add additional affiliations by running the createProjPeople() function."))
  }
  
  
  # person's role in the project----
  if(type == "new project"){
    writeLines("\nStep 3.3: Person's role")} else {
      writeLines("\nStep 2.3: Person's role")
    }
  add_role = function(){
    # query existing roles from the database
    roles <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_roles"))
    if(length(proj_id) > 1){
      message("\nPlease enter one or more roles of the person in the projects.")
    } else {
      message("\nPlease enter one or more roles of the person in the project.")
    }
    proj_role = utils::select.list(c(sort(roles$proj_people_role), "other"), multiple = T, graphics = F)
    
    # enter new role when 'other' was chosen...
    if("other" %in% proj_role){
      if(type == "new project"){
        message("\nStep 3.3.1: New role")
        } else {
          message("\nStep 2.3.1: New role")
        }
      message("\nYou could not find an appropiate role and want to add a new role.")
     new_role = readline("Enter new role title: ")
      
      # check if proj_role is empty
      if(new_role == ""){
          while(new_role == ""){
            warning("When the option 'other' was chosen you must define new role or deselect 'other'.", immediate. = T)
            rechoice = utils::select.list(c("Deselect other", "Define new role"), 
                                          title = "Please chose by typing '1' or '2':", graphics=F)
            if(rechoice == "Define new role"){
              new_role = readline("Enter new role title: ")
              } else {
              proj_role = proj_role[proj_role != "other"]
              }
          }
      }
      # write new role to database
      if((new_role %in% roles$proj_people_role) == F){
        DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_roles"), data.frame("proj_people_role" = new_role), append = T)
      }
    }
    # write proj_id, people_id and proj_role to the projects.proj_people table of the database
    if(exists("new_role")){
      proj_role = c(proj_role, new_role)
    } 
    proj_role = proj_role[proj_role != "other"]
    new_data = unique(expand.grid(proj_id = proj_id, people_id = people_id, proj_role = proj_role))
    DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_people"), new_data, append = T)
      
    if(length(proj_role) > 1){
      message(paste0(paste0("'",proj_role,"'", collapse = ", "), " were added as role of '", person, "' in the project ",paste0("'",proj_name,"'", collapse = ", "), "."))
    }
    if(length(proj_role) == 1){
      message(paste0(paste0("'",proj_role,"'", collapse = ", "), " was added as role of '", person, "' in the projects ",paste0("'",proj_name,"'", collapse = ", "), "."))
    }
      
  # run function add_role() to link a project role to a person
  add_role()
  
  
  # ask if more than one person must be added to the same project
  if(length(proj_id) == 1){
    message("\nDo you want to add another person to the same project?")
  } else {
    message("\nDo you want to add another person to the same projects?")
  }
  
  sec_person <<- utils::select.list(c("NO", "YES"), title = "Please chose by typing '1' or '2' and press 'Enter':", graphics=F) 
 }
 
 # run the select_person() function
 add_person()
 
 # re-run the select_person() function to add more persons
 while(sec_person == "YES"){
   add_person()
 } 
 if(sec_person == "NO"){
   # final message
   message(paste0("\nYou entered all relevant persons to the project '", proj_name, "'.\nYou can always add additional people, by running the createProjPeople() function."))
   if(type == "new project"){
     rm(list = ls())
   }
 }  
  
}
