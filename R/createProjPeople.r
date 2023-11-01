#' enter new project participants to the EcoDyn database
#' 
#' createProjPeople 
#' 
#' Function to add people to the EcoDyn database that participate in projects.
#' 
#' @export

createProjPeople = function(){
  
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
  
  
  people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
  
  p_filter = readline("Enter family name (partly or first letter): ")
  f_people = people[grepl(p_filter, people$family_name),]
  
  select_person = function(){
    utils::select.list(c(sort(paste(f_people$family_name, f_people$first_name, sep = ", ")), "other"), graphics = T, multiple = T)
  }
  person <- select_person()
  
  if(person == "other"){
    writeLines("You want to add a unknown person, please fill out the following:")
    family_name <- readline("1. Enter family name: ")
    first_name <- readline("2. Enter first name: ")
    
    people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
    p_check = paste(people$family_name, people$first_name, sep = ", ")
    
    if(paste(family_name, first_name, sep = ", ") %in% p_check){
      stop(paste0("'", paste(family_name, first_name, sep = ", "),"' already exists in the database. Please check!"))
    } else {
      DBI::dbWriteTable(db_con, DBI::Id(schema="people", table="people"), data.frame(first_name, family_name), append = T)
      people = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
      person_id = people$people_id[paste(people$family_name, people$first_name, sep = ", ") == paste(family_name, first_name, sep = ", ")]
    }
  }
}