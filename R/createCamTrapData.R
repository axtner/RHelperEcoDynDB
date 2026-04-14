#' import camera trapping data in the EcoDyn database
#' 
#' createCamTrapData 
#' 
#' Function to import camera trapping data, including station sites, run times and link them to a project of the EcoDyn database.
#' 
#' @export

createCamTrapData = function(proj_name){
  
  # Step 1, selecting projects ----
  # set project name and ID
  if((exists("proj_name", envir = .GlobalEnv) == T) & (exists("proj_id", envir = .GlobalEnv) == T)){
    proj_name <- get0("proj_name", envir = .GlobalEnv)
    proj_id <- get0("proj_id", envir = .GlobalEnv)
  } else {
    writeLines("\nWelcome, you want to add a camera trapping survey, import it's camera trapping data and link it to existing projects of the EcoDyn database.\nStep 1: Select projects:")
    RHelperEcoDynDB:::.selectProject()
    if(length(proj_id) > 1){
      warning(paste0("\nYou selected more than one project!\nThe data will be linked to both of your ", length(proj_id), " selected projects.\nDo you want to continue?"), immediate. = T)
      continue = utils::select.list(c("Continue", "Select again", "Exit"), title = "Please chose how to continue:", graphics=F) 
      if(continue == "Select again"){
        RHelperEcoDynDB:::.selectProject()
      }
      if(continue == "Exit"){
        message("You decided to leave... Have a nice day!")
        return(invisible(NULL))
      }
      else {
        message(paste0("You decided to continue.\nThe imported camera trapping data will linked to the following projects: \n",paste(proj_name, collapse = ", ")))
        }
    }
  }
  
  # step 2, creating camtrap survey in DB ----
  sites_db = DBI::dbReadTable(db_con, DBI::Id(schema = "geodata", table = "study_area"))
  s_filter = readline("Enter site name (partly or first letter) to restrict search: ")
  pattern = paste(trimws(strsplit(s_filter, ",")[[1]]), collapse = "|")
  sites = utils::select.list(c(sites_db$study_area[grepl(tolower(pattern), tolower(sites_db$study_area))], "Other"), multiple = TRUE, title = "Select study site", graphics=F)
  if("Other" %in% sites){
    message("You chose 'other' and want to enter one or more new study areas.\nWhen entering more than one area, separate them by ';' (e.g. 'area1;area2').")
    new_site = readline("Enter new study areas: ")
    while(new_site == ""){
      warning("\nYou did not enter a new study area. Please do so or deselect 'other'", immediate. = T)
      continue = utils::select.list(c("Enter new study area", "Deselect 'other'"), title = "Please chose wisely:", graphics=F)
      if(continue == "Enter new study area"){
        new_site = readline("Enter name of new stud area: ")
      } else {
        new_site = "nothing new"
      }
    }
    new_site = gsub("; | ;", ";", new_site)
    new_site = strsplit(new_site, ";")[[1]]
    
    sites = sites[sites != "Other"]
    if(!("nothing new" %in% new_site)){
      DBI::dbWriteTable(db_con, DBI::Id(schema = "geodata", table = "study_area"), data.frame(study_area = new_site), append = T)
      sites_db = DBI::dbReadTable(db_con, DBI::Id(schema = "geodata", table = "study_area"))
      sites = unique(c(sites, new_site))
      site_ids = sites_db$study_area_id[sites_db$study_area %in% sites]
      
    }
  }
  
  date = readline("Enter start date in formt '2026-05-31': ")
  date = RHelperEcoDynDB:::.parse_date(date)
  comment = readline("Enter any comment on survey: ")
  new_survey = data.frame(area = paste(sites, collapse ="; "), start_date = date, comment = comment )
  surveys_db = DBI::dbReadTable(db_con, DBI::Id(schema = "camtrapdata", table = "ct_surveys"))
  if(paste(new_survey$start_date, new_survey$area) %in% paste(surveys_db$start_date, surveys_db$area)){
    stop(paste0("\nThe camera trapping survey from '", date, "' for the areas '",paste(sites, collapse ="',' "),"' already exists in the EcoDyn database.\nIn case you disagree please contact the database administrators.\n"))
  }
  DBI::dbWriteTable(db_con,  DBI::Id(schema = "camtrapdata", table = "ct_surveys"), new_survey, append = T)
  
  # select ct-file
  writeLines("Select file containing project folder")
  proj_dir <- RHelperEcoDynDB:::.selectDir()
  proj_dir <- gsub("\\\\", "/", proj_dir)
  
  
}
  