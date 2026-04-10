#' import camera trapping data in the EcoDyn database
#' 
#' createCamTrapData 
#' 
#' Function to import camera trapping data, including station sites, run times and link them to a project of the EcoDyn database.
#' 
#' @export

createCamTrapData = function(proj_name){
  
  # set project name and ID ----
  if((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", envir = .GlobalEnv) == T)){
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
    type <<- "new project"
  } else {
    writeLines("\nWelcome, you want to import camera trapping data and link it to existing projects of the EcoDyn database.\nStep 1: Select projects:")
    RHelperEcoDynDB::selectProject()
    if(length(proj_id) > 1){
      warning(paste0("\nYou selected more than one project!\nThe data will be linked to both of your ", length(proj_id), " selected projects.\nDo you want to continue?"))
      continue <- 
      RHelperEcoDynDB:::.selectProject()
    }
  }
  
  # select project folder
  writeLines("Select project folder")
  proj_dir <- RHelperEcoDynDB:::.selectDir()
  proj_dir <- gsub("\\\\", "/", proj_dir)
  
  
}
  