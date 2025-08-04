#' Read project realted information from EcoDyn database
#' 
#' getProjInfo 
#' 
#' Function to reads information like persons, organisations, etc. associated with a certain project from the database.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

getProjInfo = function(){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # read proj_info table from database
  projects <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
  
  # welcome message
  writeLines("\nWelcome!\nYou are searching for a special project of the EcoDyn database.\nYou can filter the search by \n(*)part of the project name, \n(*)start year of a project, \n(*)project keywords, \n(*)name of a project participant or \n(*)an involved organisation.")
  
  # select filter
  filter <- function(){
    selFilter <- utils::select.list(c("Project name", "Start year", "Keyword", "Paricipant", "Organisation", "No filter", "Exit"), title = "Please choose from the following options:", graphics=F)
    
    # filter 1, project name ----
    if(selFilter == "Project name"){
      f1 = readline("Enter partial project name:")
      f_projects = projects[grepl(tolower(f1), tolower(projects$proj_name)),]
      f_projects = f_projects[order(f_projects$proj_name),]
      rm(f1)
    }# end of filter 1
    
    # filter 2, start year ----
    if(selFilter == "Start year"){
      f2 = readline("Enter start year of project:")
      f_projects = projects[grepl(tolower(f2), tolower(projects$proj_year)),]
      f_projects = f_projects[order(f_projects$proj_name),]
      rm(f2)
    }# end of filter 2
    
    # filter 3, keyword ----
    if(selFilter == "Keyword"){
      keywords_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
      writeLines("Select keywords related to the project:")
      f3 = utils::select.list(sort(keywords_db$keyword), graphics = F, multiple = T)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f3)){
        q_i = paste0("'", paste0("%", f3[i], "%"), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      projects_3 <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         where keyword like any (array[", q_1,"])"))
     f_projects = projects_3[order(projects_3$proj_name),]
      rm(keywords_db, f3, q_1, projects_3)
    }# end of filter 3
    
    # filter 4, participants ----
    if(selFilter == "Person"){
      persons_db = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
      persons_db = persons_db[order(persons_db$family_name),]
      name_width1 <- max(nchar(persons_db$first_name)) + 2
      name_width2 <- max(nchar(persons_db$family_name)) + 2
      writeLines("Select persons related to the project:")
      persons <- utils::select.list(sprintf(paste0("%-", name_width2, "s %", name_width1,"s %4s"), persons_db$family_name, persons_db$first_name, persons_db$people_id), multiple = T, graphics = F)
      f4 <- sub(".*\\s(\\d+)$", "\\1", persons)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f4)){
        q_i = paste0("'", paste0(f4[i]), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      projects_4 <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         left join projects.proj_people prp on prp.proj_id = pri.proj_id
         left join people.people pep on pep.people_id = prp.people_id
         where prp.people_id in (",q_1,")"
        ))
      f_projects = projects_4[order(projects_4$proj_name),]
      rm(persons_db, f4, q_1, projects_4)
    }# end of filter 4
    
    # filter 5, organisation ----
    if(selFilter == "Organisation"){
      orgs_db = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
      orgs_db = orgs_db[order(orgs_db$organisation),]
      name_width1 <- max(nchar(orgs_db$organisation)) + 2
      name_width2 <- max(nchar(orgs_db$abbreviation)) + 2
      writeLines("Select organisations related to the project:")
      orgs <- utils::select.list(sprintf(paste0("%-", name_width1, "s %", name_width2,"s %4s"), orgs_db$organisation, orgs_db$abbreviation, orgs_db$org_id), multiple = T, graphics = F)
      f5 <- sub(".*\\s(\\d+)$", "\\1", orgs)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f5)){
        q_i = paste0("'", paste0(f5[i]), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      projects_5 <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         left join projects.proj_organisations pro on pro.proj_id = pri.proj_id
         left join people.organisations peo on peo.org_id = pro.org_id
         where pro.org_id in (",q_1,")"
      ))
      f_projects = projects_5[order(projects_5$proj_name),]
      rm(persons_db, f5, q_1, projects_4)
    }# end of filter 5
    
    # prompt result of filter to choose from ----
    name_width = max(nchar(f_projects$proj_name)) + 2
    project <<- utils::select.list(sprintf(paste0("%-", name_width, "s %6s %4s"), f_projects$proj_name, f_projects$proj_year, f_projects$proj_id), graphics = F)
    proj_id <<- sub(".*\\s(\\d+)$", "\\1", project)
  }# end of filter()
  
  # run filter() function
  filter()
  
  # query project information from database
  # project people ----
  p_people <- DBI::dbGetQuery(db_con, paste0(
    "select distinct
     family_name, 
     first_name,
     proj_role
     from projects.proj_info pri
     left join projects.proj_people prp on prp.proj_id = pri.proj_id
     left join people.people pep on pep.people_id = prp.people_id
     where pri.proj_id = ", proj_id
  ))
  p_people <- p_people[order(p_people$family_name, p_people$first_name, p_people$proj_role),]
  if(!(is.na(p_people$family_name[1]))){
    p_people$full_name <- paste(p_people$family_name, p_people$first_name, sep = ", ")
    agg1 <- aggregate(proj_role ~ full_name, data = p_people, FUN = function(x) paste(x, collapse = ", "))
    name_width1 <- max(nchar(agg1$full_name)) + 2
  }
  
  # project organisations ----
  p_orgs <- DBI::dbGetQuery(db_con, paste0(
    "select distinct 
     organisation, 
     abbreviation,
     proj_org_role     
     from projects.proj_info pri
     left join projects.proj_organisations pro on pro.proj_id = pri.proj_id     
     left join people.organisations peo on peo.org_id = pro.org_id     
     where pri.proj_id = ", proj_id
  ))
  p_orgs <- p_orgs[order(p_orgs$organisation, p_orgs$abbreviation, p_orgs$proj_org_role),]
  if(!(is.na(p_orgs$organisation[1]))){
    p_orgs$org_name <- paste(p_orgs$organisation, p_orgs$abbreviation, sep = ", ")
    agg2 <- aggregate(proj_org_role ~ org_name, data = p_orgs, FUN = function(x) paste(x, collapse = ", "))
    name_width2 <- max(nchar(agg2$org_name)) + 2
  }
  
  # project keywords ----
  p_keys <- DBI::dbGetQuery(db_con, paste0(
    "select distinct
     keyword
     from projects.proj_info pri
     left join 
     projects.proj_keywords prk on prk.proj_id = pri.proj_id
     where pri.proj_id = ", proj_id
  ))
  
  
  writeLines(paste0("\nInformation on project '", projects$proj_name[projects$proj_id == proj_id], "'"))
  writeLines(paste0("\nStart year: ", projects$proj_year[projects$proj_id == proj_id]))
  writeLines("\nPersons involved in the project and their role:")
  if(!(is.na(p_people$family_name[1]))){
    writeLines(sprintf(paste0("%-", name_width1, "s %4s"), agg1$full_name, agg1$proj_role))
  } else {
    writeLines("No persons documented in the EcoDynDB for this project.")
  }
  writeLines("\nOrganisations involved in the project and their role:")
  if(!(is.na(p_orgs$organisation[1]))){
    writeLines(sprintf(paste0("%-", name_width2, "s %4s"), agg2$org_name, agg2$proj_org_role))
  } else {
    writeLines("No organistions documented in the EcoDynDB for this project.")
  }
  writeLines("\nKeywords associated with this project:")
  if(nrow(p_keys) != 0) {
    writeLines(paste(p_keys$keyword))
  } else {
    writeLines("No keywords documented in the EcoDynDB for this project.")
  }
  writeLines("\nProject description:")
  writeLines(paste(projects$proj_description[projects$proj_id == proj_id]))
  
  
}# end of getProjInfo()
