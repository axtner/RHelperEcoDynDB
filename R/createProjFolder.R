#' create new entry for project folder and folder content in the EcoDyn database
#' 
#' createProjFolder 
#' 
#' Function to create new entry for project folder and folder content in the EcoDyn database.
#' 
#' @export

createProjFolder = function(proj_name){

  # set project name and ID ----
  if((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", envir = .GlobalEnv) == T)){
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
    type <<- "new project"
  } else {
    writeLines("\nWelcome, you want to link a folder and a list of folder content to an existing project.\nStep 1: Select project:")
    RHelperEcoDynDB::selectProject()
    while(length(proj_id) > 1){
      warning(paste0("\nTo link a folder to a existing project you are not allowed to select more than one project.\nCurrently you selected ", length(proj_id), ",  please select again."))
      RHelperEcoDynDB::selectProject()
      }
    }
  
  # select project folder
  writeLines("Select project folder")
  proj_dir <- RHelperEcoDynDB:::.selectDir()
  proj_dir <- gsub("\\\\", "/", proj_dir)
  
  # files in main folder
  files_lvl0 <- list.files(proj_dir, full.names = TRUE)
  
  # sub-dirs of 1st order
  subdirs_lvl1 <- list.dirs(proj_dir, recursive = FALSE, full.names = TRUE)
  
  # files in 1st order sub-dirs
  files_lvl1 <- unlist(
    lapply(subdirs_lvl1, list.files, full.names = TRUE)
  )
  
  # all files and folders
  files <- c(files_lvl0, files_lvl1)
  
  # file information
  info <- file.info(files)
  
  # relative path
  rel_path <- substring(files, nchar(proj_dir) + 2)
  type <- ifelse(info$isdir, "folder", "file")
  
  # Tabelle
  tab <- data.frame(
    name = basename(files),
    type = type,
    rel_path = rel_path,
    size = info$size,
    stringsAsFactors = FALSE
  )
   tab = tab[order(tab$rel_path, tab$type),]
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
   
  # write project folder to projects.proj_folders
  DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_folders"), data.frame(proj_id=proj_id, path=proj_dir), append = T)
   
  # get ID from projects.proj_folders
  db_folders = DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_folders")
  folder_id = db_folders$path_id[db_folders$path == proj_dir]
  tab$path_id =folder_id
   
  # write folder content to projects.proj_folders_content
  DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_folders_content"), tab, append = T)
}