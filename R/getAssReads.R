#' create get assigned reads from the EcoDyn database
#' 
#' getAssReads 
#' 
#' Function to queryread assigned reads from the EcoDyn database based on sample name, spies, genus or family and save them as fasta file.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

getAssReads <- function(){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # welcome message
  writeLines("\nWelcome!\nYou want to query reads that were assigned to certain taxa from the EcoDyn database. \nYou will be able to filter the search by \n(*)genetic marker, \n(*)project, \n(*)sample type, \n(*)study site, \n(*)taxonomic unit or \n(*)read number.\n\nYou can further decide to exclude all reads assigned to human or positive controls.\nResults will be exported as alignment to a fasta file.")
  message("\nPlease note that the results you will receive are raw assignment results, without any editing.\nFinal acceptance as an assignment for a sample must always be made by careful visual inspection of the results.")  
  
  filter <- function(){
    selFilter <<- utils::select.list(c("Genetic marker", "Project", "Sample type", "Study site", "Taxonomic unit", "Number reads", "Exit"), title = "Please choose from the following options:", graphics=F, multiple = T)
    
    # gen marker
    if("Genetic marker" %in% selFilter){
      marker_db <- DBI::dbGetQuery(db_con, "SELECT DISTINCT gen_marker FROM sfb.reads")
      marker <<- utils::select.list(marker_db$gen_marker, title = "Select genetic marker:")
    }# end of gen marker
    
    # project
    if("Project" %in% selFilter){
      projects <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
      proj_ids = unique(assignments$proj_id)
      projects = projects[projects$proj_id %in% proj_ids,]
      name_width = max(nchar(projects$proj_name)) + 2
      project <<- utils::select.list(sprintf(paste0("%-", name_width, "s %6s %4s"), projects$proj_name, projects$proj_year, projects$proj_id), graphics = F, multiple = T)
      proj_id <<- sub(".*\\s(\\d+)$", "\\1", project)
      rm(proj_ids, projects, name_width)
    }# end of project
  } # end of filter()
  
} # end of getAssReads()