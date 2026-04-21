#' import camera trapping data in the EcoDyn database
#' 
#' createCamTrapData 
#' 
#' Function to import camera trapping data, including station sites, run times and link them to a project of the EcoDyn database.
#' 
#' @export

createCamTrapSurvey = function(proj_name){
  
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
  
  # study areas ----
  message("\nSeletc study areas used for this survey.\nYou can filter for certain areas using parts or first letter of their name.\nWhen using multiple filters, separate them by comma, e.g. 'forest,national,park'.")
  areas_db = DBI::dbReadTable(db_con, DBI::Id(schema = "geodata", table = "study_area"))
  a_filter = readline("Enter filter to restrict area search: ")
  a_filter = gsub(", | ,", ",", a_filter)
  pattern = paste(trimws(strsplit(a_filter, ",")[[1]]), collapse = "|")
  areas = utils::select.list(c(areas_db$study_area[grepl(tolower(pattern), tolower(areas_db$study_area))], "Other"), multiple = TRUE, title = "Select study area", graphics=F)
  if("Other" %in% areas){
    message("You chose 'other' and want to enter one or more new study areas.\nWhen entering more than one area, separate them by ';' (e.g. 'area1;area2').")
    new_area = readline("Enter new study areas: ")
    while(new_area == ""){
      warning("\nYou did not enter a new study area. Please do so or deselect 'other'", immediate. = T)
      continue = utils::select.list(c("Enter new study area", "Deselect 'other'"), title = "Please chose wisely:", graphics=F)
      if(continue == "Enter new study area"){
        new_area = readline("Enter name of new stud area: ")
      } else {
        new_area = "nothing new"
      }
    }
    new_area = gsub("; | ;", ";", new_area)
    new_area = strsplit(new_area, ";")[[1]]
    
    areas = areas[areas != "Other"]
    if(!("nothing new" %in% new_site)){
      DBI::dbWriteTable(db_con, DBI::Id(schema = "geodata", table = "study_area"), data.frame(study_area = new_site), append = T)
      areas_db = DBI::dbReadTable(db_con, DBI::Id(schema = "geodata", table = "study_area"))
      areas = unique(c(areas, new_area))
    }  
  }
  area_ids = areas_db$study_area_id[sites_db$study_area %in% sites]
  rm(c(a_filter, pattern))
  
  # survey dates ----
  start_date = readline("Enter start date in formt '2026-05-31': ")
  start_date = RHelperEcoDynDB:::.parse_date(start_date)
  
  end_date = readline("Enter start date in formt '2026-05-31': ")
  end_date = RHelperEcoDynDB:::.parse_date(end_date)
  
  # survey type ----
  types_db = DBI::dbReadTable(db_con, DBI::Id(schema = "surveydata", table = "types"))
  message("\nSeletc survey types (e.g. 'telemetry', 'camera trapping').\nYou can filter by using parts or first letter of the survey types.\nWhen using multiple filters, separate them by comma, e.g. 'cam,DNA,tele'.")
  t_filter = readline("Enter filter to restrict type search: ")
  t_filter = gsub(", | ,", ",", t_filter)
  pattern = paste(trimws(strsplit(t_filter, ",")[[1]]), collapse = "|")
  types = utils::select.list(c(types_db$survey_type[grepl(tolower(pattern), tolower(types_db$survey_type))], "Other"), multiple = TRUE, title = "Select survey type", graphics=F)
  if("Other" %in% types){
    message("You chose 'other' and want to enter one or more new survey types.\nWhen entering more than one type, separate them by ';' (e.g. 'type1;type2').")
    new_type = readline("Enter new survey types: ")
    while(new_type == ""){
      warning("\nYou did not enter a new survey type. Please do so or deselect 'other'", immediate. = T)
      continue = utils::select.list(c("Enter new survey type", "Deselect 'other'"), title = "Please chose wisely:", graphics=F)
      if(continue == "Enter new survey type"){
        new_type = readline("Enter name of new survey type: ")
      } else {
        new_type = "nothing new"
      }
    }
    new_type = gsub("; | ;", ";", new_type)
    new_type = strsplit(new_type, ";")[[1]]
    
    types = types[types != "Other"]
    if(!("nothing new" %in% new_type)){
      DBI::dbWriteTable(db_con, DBI::Id(schema = "surveydata", table = "types"), data.frame(survey_type = new_type), append = T)
      types_db = DBI::dbReadTable(db_con, DBI::Id(schema = "surveydata", table = "types"))
      types = unique(c(types, new_type))
    }  
  }
  type_ids = types_db$surveytype_id[types_db$survey_type %in% types]
  rm(c(t_filter, pattern))
  
  # comment on survey ----
  comment = readline("Enter any comment on survey: ")
  
  
  
  new_survey = data.frame(area = paste(sites, collapse ="; "), start_date = start_date, end_date = end_date, comment = comment )
  
  # read existing camtrap surveys from DB
  surveys_db = DBI::dbReadTable(db_con, DBI::Id(schema = "surveydata", table = "survey_info"))
  # compare new survey with existing ct-surveys of DB and stop if it already exists
  if(paste(new_survey$start_date, new_survey$area) %in% paste(surveys_db$start_date, surveys_db$area)){
    stop(paste0("\nThe camera trapping survey from '", date, "' for the areas '",paste(sites, collapse ="',' "),"' already exists in the EcoDyn database.\nIn case you disagree please contact the database administrators.\n"))
  }
  # writing new ct-survey to the camtrapdata.ct_surveys table
  DBI::dbWriteTable(db_con,  DBI::Id(schema = "surveydata", table = "survey_info"), new_survey, append = T)
  
  # retrieve survey_id of the new ct-survey from the DB
  surveys_db = DBI::dbReadTable(db_con, DBI::Id(schema = "surveydata", table = "survey_info"))
  survey_id = surveys_db$survey_id[paste(new_survey$start_date, new_survey$area) %in% paste(surveys_db$start_date, surveys_db$area)]
  
  # writing to the surveydata.surveys_areas table that links study areas with ct-surveys
  DBI::dbWriteTable(db_con,  DBI::Id(schema = "surveydata", table = "surveys_areas"), data.frame(expand.grid(survey_id = survey_id, study_area_id = site_ids)), append = T)
  
  # writing to the surveydata.survey_type table that links survey with survey types
  DBI::dbWriteTable(db_con,  DBI::Id(schema = "surveydata", table = "surveys_type"), data.frame(expand.grid(survey_id = survey_id, surveytype_id = type_ids)), append = T)
  
  
  # select ct-file
  message("Select file containing station names, coordinates, set-up and retrieval times and dates, as well as a column with dates of last image or error to allow runtime calculations for each camera.\nFiles can be of '.xlxs', '.csv', '.txt' or '.xls' format")
  ct_dir <- RHelperEcoDynDB:::.selectDir()
  ct_dir <- gsub("\\\\", "/", ct_dir)
  
  if(exists("ct_dir")){
    file_name <- utils::select.list(list.files(ct_dir, pattern ="xlxs|csv|txt|xls"), title = "Select input file", graphics=F)
    file_path <- list.files(ct_dir, full.names = T, pattern = file_name)
    #[grepl(in_file, list.files(in_dir, full.names = T))]
    file_path <- gsub("\\\\", "/", file_path)
    if(length(file_path) != 1){
      file_path <- utils::select.list(file_path, title = "Sorry, please select the correct file:")
    }
  }
  
  # read in EXEL spreadsheets
  if(grepl(".xls", file_path)){
    message("Please choose the Excel sheet containing the point geometries.")
    sheet <- utils::select.list(readxl::excel_sheets(file_path), graphics=F)
    writeLines("\nDefine the cell range to read from (e.g.\"B3:D87\").")
    range <- readline("Cell range: ")
    ct_table <- readxl::read_excel(file_path, sheet = sheet, range = range)
    ct_table <- data.frame(ct_table)
  }
  
  # read in other table formats
  if(grepl(".csv", file_path)){
    ct_table <- utils::read.csv(file_path)
  }
  if(grepl(".txt", file_path)){
    ct_table <- utils::read.delim(file_path)
  }
  
  # table check and ask to continue
  message(paste0("The loaded table has ", nrow(ct_table), " lines and looks like the following:"))
  print(ct_table[1:5,])
  continue <- utils::select.list(c("Yes", "No, let me select again", "Exit"), title = "Is everything correct?", graphics=F)
  while(continue == "No, let me select again"){
    file_path <- utils::select.list(file_path, title = "Sorry, please select the correct file:")
  }
  if(continue == "Exit"){
    # deleting previous entries from DB when exiting
    DBI::dbExecute(db_con, paste0("DELETE FROM camtrapdata.ct_surveys WHERE survey_id = ", survey_id))
    message("Something is wrong with the input file and you decided to leave... Have a nice day!")
    return(invisible(NULL))
  }
    
  # station or point names
  message("Select column containing station or point names")
  pointnames <- utils::select.list(names(ct_table), graphics=F)
  point_name <- tolower(ct_table[[pointnames]])
  
  # geometries
  message("Select columns containing coordinates")
  long <- utils::select.list(names(ct_table), title = "Longitude (x)", graphics=F)
  lat <- utils::select.list(names(ct_table), title = "Latitude (y)", graphics=F)
  
  if(nrow(ct_table[is.na(ct_table[[long]]) | is.na(ct_table[[lat]]),]) > 0){
    warning(paste0("\nThere were ", nrow(ct_table[is.na(ct_table[[long]]) | is.na(ct_table[[lat]]),]), " stations without full coordinates.\nThese camera trap stations will have no geometries in the database, which might affect future database queries, please check!"), immediate. = TRUE)
    continue <- utils::select.list(c("Yes", "No, let me check and exit"), title = "Do yo want to continue?", graphics=F)
    if(continue == "No, let me first check and exit for now"){
      # deleting previous entries from DB when exiting
      DBI::dbExecute(db_con, paste0("DELETE FROM surveydata.survey_info WHERE survey_id = ", survey_id))
      message("Something is wrong with the coordinates and you decided to leave... Have a nice day!")
      return(invisible(NULL))
    }
  }
  # create wkt geometries
  writeLines("Creating wkt geometries...")
  long_val <- as.numeric(ct_table[[long]])
  lat_val <- as.numeric(ct_table[[lat]])
  geom <- wk::wkt(paste0("POINT(", long_val, " ", lat_val, ")"))
  
  message("Are these WGS84 or UTM coordinates?")
  crs <- utils::select.list(c("WGS84", "UTM"), graphics=F)
  if(crs == "UTM"){
    continue = "No"
    find_crs = function(){
      zone <- readline("Enter the UTM zone (e.g. 48): ")
      sel_crs <<- DBI::dbGetQuery(db_con, paste0("SELECT proj4text, auth_name, srid                                                FROM spatial_ref_sys WHERE auth_name = 'EPSG' AND auth_srid = 326", zone, ";"))
      writeLines(paste0("\nSuggested CRS :", sel_crs$proj4text))
      continue <<- utils::select.list(c("Yes", "No"), title = "\nIs the suggested CRS correct?", graphics=F)
      if(continue == "No"){
        writeLines("Do you know the correct CRS and want to change it, or do you want to exit?")
        clue <-  utils::select.list(c("Yes, I know it, let me change it", "I have no clue, let me exit and get more info"), graphics=F)
        if(clue == "I have no clue, let me exit and get more info"){
          # deleting previous entries from DB when exiting
          DBI::dbExecute(db_con, paste0("DELETE FROM surveydata.survey_info WHERE survey_id = ", survey_id))
          message("For now you will exit. Little homework for you: find out the correct CRS. Have a nice day!")
          return(invisible(NULL))
        }
      }
    }
    while(continue == "No"){find_crs()}
    crs_id <<- sel_crs$srid
  }
  
  # set-up and retrieval dates of camera traps, as well as last image or error column
  message("Select set-up and retrieval times and dates.\nIf available select a column with dates of last images or errors to allow more accurate runtime calculations.")
  ask_dates = function(){
    setup_date <<- utils::select.list(c(names(ct_table), NA), title = "Select set_up date", graphics=F)
    ct_table[[setup_date]] <- gsub("/", "-", ct_table[[setup_date]])
    setup_time <<- utils::select.list(c(names(ct_table), NA), title = "Select set_up time", graphics=F)
    ct_table[[setup_time]] <- gsub("/", "-", ct_table[[setup_time]])
    
    retr_date <<- utils::select.list(c(names(ct_table), NA), title = "Select retrival date", graphics=F)
    ct_table[[retr_date]] <- gsub("/", "-", ct_table[[retr_date]])
    retr_time <<- utils::select.list(c(names(ct_table), NA), title = "Select retrieval time", graphics=F)
    ct_table[[retr_time]] <- gsub("/", "-", ct_table[[retr_time]])
    
    last_date <<- utils::select.list(c(names(ct_table), NA), title = "Select last image or error date", graphics=F)
    ct_table[[last_date]] <- gsub("/", "-", ct_table[[last_date]])
    date_col = c(setup_date, setup_time, retr_date, retr_time, last_date)
  }
  ask_dates()
  while(is.na(setup_date) | is.na(retr_date)){
    warning("At least set-up and retrieval dates are mandatory to allow runtime calculations.\nDo you want to reselect date columns or do you wish to exit?\nIn case you exit, you contact the database admins to get advice.")
    continue <- utils::select.list(c("Yes, let me select again", "No, let me check and exit"), graphics=F)
    if(continue == "No, let me check and exit"){
      # deleting previous entries from DB when exiting
      DBI::dbExecute(db_con, paste0("DELETE FROM surveydata.survey_info WHERE survey_id = ", survey_id))
      message("You don't have set-up and retrieval dates and decided to leave... Have a nice day!")
      return(invisible(NULL))
    } else {
      ask_dates()
    }
  }
  
  message("Present date formats:")
  print(unique(ct_table[, names(ct_table) %in% date_col]))
  date_form <- utils::select.list(c("2025-05-30", "30-05-2025", "05-30-2025", "Friday, 30 May 2025", "diff. formats, unknown"), title = "What date format is used?", graphics=F)
    
  if(date_form == "diff. formats, unknown"){
    # deleting previous entries from DB when exiting
    DBI::dbExecute(db_con, paste0("DELETE FROM surveydata.survey_info WHERE survey_id = ", survey_id))
    message("Please find out what date format you used or contact the database admins. For now your journey ends... Have a nice day!")
    return(invisible(NULL))
  }
  if(date_form == "30-05-2025"){
    ct_table[,c(setup_date, retr_date)] = lapply(ct_table[,c(setup_date, retr_date)], as.Date, format = "%Y-%m-%d")
    set_up_date = ct_table[,retr_date]
    retrieval_date = ct_table[,retr_date]
    if(last_date == "NA"){
      stop_date = retrieval_date
    } else {
      ct_table[,last_date] <- as.Date(ct_table[,last_date], format = "%Y-%m-%d")
      ct_table[,last_date][is.na(ct_table[,last_date])] = ct_table[,retr_date][is.na(ct_table[,last_date])]
      stop_date = ct_table[,last_date]
    }
  }
  if(date_form == "30-05-2025"){
    ct_table[,c(setup_date, retr_date)] = lapply(ct_table[,c(setup_date, retr_date)], as.Date, format = "%d-%m-%Y")
    set_up_date = ct_table[,retr_date]
    retrieval_date = ct_table[,retr_date]
    if(last_date == "NA"){
      stop_date = retrieval_date
    } else {
      ct_table[,last_date] <- as.Date(ct_table[,last_date], format = "%d-%m-%Y")
      ct_table[,last_date][is.na(ct_table[,last_date])] = ct_table[,retr_date][is.na(ct_table[,last_date])]
      stop_date = ct_table[,last_date]
    }
  }
  if(date_form == "05-30-2025"){
    ct_table[,c(setup_date, retr_date)] = lapply(ct_table[,c(setup_date, retr_date)], as.Date, format = "%m-%d-%Y")
    set_up_date = ct_table[,retr_date]
    retrieval_date = ct_table[,retr_date]
    if(last_date == "NA"){
      stop_date = retrieval_date
    } else {
      ct_table[,last_date] <- as.Date(ct_table[,last_date], format = "%m-%d-%Y")
      ct_table[,last_date][is.na(ct_table[,last_date])] = ct_table[,retr_date][is.na(ct_table[,last_date])]
      stop_date = ct_table[,last_date]
    }
  }
    
  if(date_form == "Friday, 30 May 2025"){
    gps_time <- lubridate::parse_date_time(
      dates,
      orders = c("Y-m-d H:M", "Y-m-d", "d B Y"),
      locale = "C"
    )
  }
  
  
  
} #end of createCamTrapData function
  