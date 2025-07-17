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
    }# end of filter 1
    
    # filter 2, start year ----
    if(selFilter == "Start year"){
      f2 = readline("Enter start year of project:")
      f_projects = projects[grepl(tolower(f2), tolower(projects$proj_year)),]
      f_projects = f_projects[order(f_projects$proj_name),]
    }# end of filter 2
    
    # filter 3, keyword ----
    if(selFilter == "Keyword"){
      keywords_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
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
         left join projects.proj_people prp on prp.proj_id = pri.proj_id
         where keyword like any (array[", q_1,"])"))
      f_projects = projects_3[grepl(tolower(f3), tolower(projects$proj_year)),]
      f_projects = f_projects[order(f_projects$proj_name),]
    }# end of filter 3
    
    # prompt result of filter to choose from
    name_width <- max(nchar(f_projects$proj_name)) + 2
    project <- utils::select.list(sprintf(paste0("%-", name_width, "s %6s %4s"), f_projects$proj_name, f_projects$proj_year, f_projects$proj_id))
    proj_id <- tail(strsplit(project, "\\s\\s\\s\\s")[[1]],1)
  }# end of filter()
  
}# end of getProjInfo()
