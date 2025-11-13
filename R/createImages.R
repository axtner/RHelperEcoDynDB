#' add images from Camtrap folders to EcoDynDB
#' 
#' createImages 
#' 
#' Function to read in camera trapping images from folders. The folder structure must correspond to the one created by camtrapR.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

createImages <- function(in_dir = NA){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # welcome message
  writeLines("\nWelcome!\nYou want to enter camera-trapping images to the EcoDyn database. \n1.You have to chose a project in  which context the photos were taken. \n2.Chose persons or organisations that have ownership on the images. \n3.You will have to chose parent folder that contains two folders 'raw' and 'identified'.\nParallelisation is used to process the images.\na ReadMe.txt file is written to the input directory with information on the dataset.\n")
  
  
  
  
  # select project ----
  projects <<- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
  project <<- utils::select.list(paste(projects$proj_name, projects$proj_year), 
                                 title = "1. Select project by choosing it's according number or '0' for exiting:", graphics = FALSE)
  project <<- projects[paste(projects$proj_name, projects$proj_year) == project,]
  proj_name <<- project$proj_name
  proj_id <<- project$proj_id
  proj_year <<- project$proj_year
  if(length(project) == 1){
    rm(project)
    stop("\nYou decided to exit. Bye!")
  }
  
    
  
  
  # select persons or organisations that have ownership on the images ----
  org_peo <- utils::select.list(c("Organisations", "Persons", "None"), 
                                title = "2. Select entities that have ownership on images:", multiple = F, graphics = FALSE)
  
  if(org_peo == "Organisations"){
    org_ownerships <<- data.frame(proj_id = integer(0), project_name = character(0), org_id = integer(0), organisation = character(0), abbreviation = character(0))
    add_org = function(){
      orgs = DBI::dbGetQuery(db_con, paste0("SELECT distinct po.org_id, organisation, abbreviation FROM people.organisations po left join projects.proj_organisations ppo on ppo.org_id = po.org_id where ppo.proj_id = ", proj_id))
      writeLines("The following organisations are part of the project.")
      org <- utils::select.list(c(sort(paste(orgs$organisation, orgs$abbreviation, sep = ", ")), "other"), graphics=F)
      
      # run createProjOrg() when 'other' was chosen
      if("other" %in% org){
        # message
        message("\nObviously your wanted organisation has yet not been linked to the project and thus you chose 'other'. You will be redirected to link a organsiation to the project.\nPlease follow the instructions and add the organisation to the project.")
        # run createProjOrg()
        RHelperEcoDynDB::createProjOrg(proj_name = get0("proj_name", envir = .GlobalEnv))
        
        # query organisations with the now hopefully added organisation
        orgs = DBI::dbGetQuery(db_con, paste0("SELECT distinct po.org_id, organisation, abbreviation FROM people.organisations po left join projects.proj_organisations ppo on ppo.org_id = po.org_id where ppo.proj_id = ", proj_id))
        writeLines("Please check again if the organisation is now part of the project.")
        org <<- utils::select.list(c(sort(paste(orgs$organisation, orgs$abbreviation, sep = ", ")), "other"), graphics=F)
        } # end of if clause 'other'
      
      organisation <- strsplit(org, ", ")[[1]][1]
      org_id <- orgs$org_id[orgs$organisation == organisation]
      abbreviation <- orgs$abbreviation[orgs$org_id == org_id]
      org_ownerships <<- rbind(org_ownerships, data.frame(proj_id, proj_name, org_id, organisation, abbreviation))
      writeLines(paste0("Chosen organsiation: ", org))
      
      continue <<- utils::select.list(c("NO", "YES"), title = "Is there another organisation with ownership on this data?", graphics=F)
    } # end of function add_org
    
    # run add_org() function
    add_org()
    while(continue == "YES"){
      add_org()
    }
  } # end of if clause 'organisation'
  
  
  if(org_peo == "Persons"){
    pers_ownerships <<- data.frame(proj_id = integer(0), project_name = character(0), people_id = integer(0), family_name = character(0), first_name = character(0))
    add_pers = function(){
      persons = DBI::dbGetQuery(db_con, paste0("SELECT DISTINCT pp.people_id, family_name, first_name FROM people.people pp LEFT JOIN projects.proj_people ppp ON ppp.people_id = pp.people_id WHERE ppp.proj_id = ", proj_id))
      writeLines("The following persons are part of the project. Please select person by typing the according number of the first column.")
      person <- utils::select.list(c(sort(paste(persons$family_name, persons$first_name, persons$people_id, sep = ", ")), "other"), graphics=F)
      
      # run createProjPeople() when 'other' was chosen
      if("other" %in% person){
        # message
        message("\nObviously your wanted person has yet not been linked to the project and thus you chose 'other'. You will be redirected to link a organsiation to the project.\nPlease follow the instructions and add the organisation to the project.")
        # run createProjOrg()
        RHelperEcoDynDB::createProjPeople(proj_name = get0("proj_name", envir = .GlobalEnv))
        
        # query persons with the now hopefully added person
        persons = DBI::dbGetQuery(db_con, paste0("SELECT DISTINCT pp.people_id, family_name, first_name FROM people.people pp LEFT JOIN projects.proj_people ppp ON ppp.people_id = pp.people_id WHERE ppp.proj_id = ", proj_id))
        writeLines("Please check again if the person is now part of the project.Please select person by typing the according number of the first column.")
        person <<- utils::select.list(c(sort(paste(persons$family_name, persons$first_name, persons$people_id, sep = ", ")), "other"), graphics=F)
      } # end of if clause 'other'
      
      family_name <- strsplit(person, ", ")[[1]][1]
      first_name <- strsplit(person, ", ")[[1]][2]
      people_id <- strsplit(person, ", ")[[1]][3]
      pers_ownerships <<- rbind(pers_ownerships, data.frame(proj_id, proj_name, people_id, family_name, first_name))
      writeLines(paste0("Chosen person: ", person))
      
      continue <<- utils::select.list(c("NO", "YES"), title = "Is there another person with ownership on this data?", graphics=F)
    }
    
    # run add_org() function
    add_org()
    while(continue == "YES"){
      add_pers()
    }
  }
  
  
  
  
  # select output directory ----
  writeLines("\n3. Please select local input directory from the dialog window.")
  if(is.na(in_dir)){
    in_dir <<- utils::choose.dir(default = "Computer", "Select folder to import images from")
    # test if 'raw' and 'identified' folders exist
  }
  if(("raw" %in% list.dirs(in_dir, full.names = F, recursive = F)) == F | ("identified" %in% list.dirs(in_dir, full.names = F, recursive = F)) == F){stop("Either the 'raw' or the 'identified' folder is missing in your selected folder!")}
  # polishing the patha bit
  in_dir = gsub("\\\\", "/", in_dir)
  
  # writing path to camtrapdata.ct_folders in EcoDynDB
  DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "ct_folders"), data.frame(path = in_dir, proj_id = proj_id), append = T)
  
  
  
  
  # process images of 'raw' folder ----
  writeLines("\n4. Processing the raw images\nDuration of the processing depends on the number of images.")
  
  # read in point_ids for ct_station for this project
  point_ids <<- DBI::dbGetQuery(db_con, paste0("SELECT gp.point_id, point_name from geodata.points gp left join projects.proj_sites pps on gp.point_id = pps.point_id where proj_id = ", proj_id))
  
  # Day folders (all folders in raw/*/*/*)
  raw_stations <- list.dirs(file.path(in_dir, "raw"), recursive = FALSE)
  
  writeLines("Reading the daily folders containing the rawimages....")
  day_dirs <<- unlist(lapply(raw_stations, function(station) {
    cameras <<- list.dirs(station, recursive = FALSE)
    unlist(lapply(cameras, function(camera) {
      list.dirs(camera, recursive = FALSE)
    }))
  }))
  writeLines(paste0("\nNumber of ct-stations: ", length(raw_stations)))
  writeLines(paste0("\nNumber of day folders with images to process: ", length(day_dirs)))
  
  
  # function to process day folders with raw images
  read_day_folder <<- function(day_path) {
    img_files <- list.files(day_path, pattern = "\\.jpe?g$", full.names = TRUE, ignore.case = TRUE)
    if (length(img_files) == 0) return(NULL)
  
    meta <- tryCatch(exif_read(img_files), error = function(e) NULL)
    if (is.null(meta)) return(NULL)
  
    # Auf bekannte Spalten beschränken
    known_fields <- c("FileName", "SourceFile", "FileSize", "FileAccessDate", "ImageDescription", "Make", "Model", "Software", "ModifyDate", "DateTimeOriginal", "CreateDate", "Flash", "ImageWidth", "ImageHeight", "Megapixels")
    meta <- meta[names(meta) %in% known_fields]
  
    # get temperature from metadata
    pattern <- "Temperature=\\s*([-+]?[0-9]*\\.?[0-9]+)"
    meta$temperature <- as.numeric(sub(".*Temperature=\\s*([-+]?[0-9]*\\.?[0-9]+).*", "\\1", meta$ImageDescription))
  
    # adding the ct_station
    meta$ct_station <- tolower(lapply(strsplit(as.character(day_path),"/"), "[[", 3))
  
    # adding the point_ids
    meta <- merge(meta, point_ids, by.x = "ct_station", by.y = "point_name", all.x =T, all.y = F)
  
    as.data.frame(meta, stringsAsFactors = FALSE)
  }
  
  # start cluster for parallelization
  num_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(num_cores)
  
  writeLines(paste0("\nCreating cluster for parallel processing, using ", num_cores, " of threads"))
  parallel::clusterEvalQ(cl, library(exiftoolr))
  parallel::clusterExport(cl, varlist = c("day_dirs", "read_day_folder", "point_ids"))
  
  # read the meta data of jpegs in parallel
  writeLines(paste0("\nProcessing of all raw images in parallel.\nThis might take a while, better go and get some coffee.\nPlease be patient and wait...."))
  meta_list <- parallel::parLapply(cl, day_dirs, read_day_folder)
  
  # stop cluster
  parallel::stopCluster(cl)
  
  # remove NULLs and combine results
  meta_list <- Filter(Negate(is.null), meta_list)
  raw_images <- do.call(rbind, meta_list)
  
  # add proj_id
  raw_images$proj_id = proj_id
  
  # calculate image hashes for image comparison
  calc_hashes <<- function(folder) {
    output <- tempfile(fileext = ".csv")
    script_path <- system.file("python", "hash_images.py", package = "RHelperEcoDynDB")
    
    system2("python", args = c(shQuote(script_path), shQuote(folder), shQuote(output)))
    
    if (file.exists(output)) read.csv(output, stringsAsFactors = FALSE) else NULL
  }
  
  # start cluster and export objects and libs
  cl <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cl,"calc_hashes")
  # calculate hashes per day folder in python
  hash_results <- parallel::parLapply(cl, day_dirs, calc_hashes)
  parallel::stopCluster(cl)
  
  # collect results of nodes and polish path
  hash_df <- do.call(rbind, hash_results)
  hash_df$path = normalizePath(hash_df$path, winslash = "/", mustWork = FALSE)
  
  # adding hashes to raw_images
  raw_images = merge(raw_images, hash_df, by.x = "SourceFile", by.y = "path", all.x=T)
  
  # commenting on damaged images where no hashes could be calculated
  raw_images$comment = NA
  raw_images$comment[is.na(test$sha256)] = "damaged image"
  
  #adding db user how uploaded the data
  raw_images$added_by = DBI::dbGetInfo(db_con)$username
  
  
  names(raw_images) <- c("file_path", "ct_station", "file_name", "file_size", "added_db", "description", "cam_type", "cam_id", "software", "date_modify", "date_original", "date_create", "flash", "img_width", "img_height", "megapixel", "temperature", "point_id", "proj_id", "sha256", "ahash", "dhash", "phash", "comment", "added_by")
  
  # write new images to DB
  writeLines("\nWriting raw images to database....")
  DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "raw_images"), raw_images, append = T)
  # get img_id from database
  db_raw_images <- DBI::dbGetQuery(db_con, paste0("SELECT img_id, sha256 from camtrapdata.raw_images ri where ri.proj_id = ", proj_id))
  # restrict to images f raw_images from this session
  raw_images = merge(raw_images[!(is.na(raw_images$sha256)),], db_raw_images, by=c("sha256"), all.x = T, all.y=F)
  raw_images = raw_images[,names(raw_images) %in% c("ct_station","date_original","cam_id","file_size","sha256", "ahash", "dhash", "phash","proj_id","img_id","point_id")]
  # damages images without hashes
  no_hash_raw = raw_images[is.na(raw_images$sha256),]
  rm(db_raw_images, hash_df, hsah_results)
  
  
  
  
  # process images of 'identified' folder ----
  writeLines("\n5. Processing the identified images\nDuration of the processing depends on the number of images.")
  
  # station folders (all folders in identified/*/*/*)
  ident_stations <- list.dirs(file.path(in_dir, "identified"), recursive = FALSE)
  
  # species folders (all folders in identified/*/*/*)
  ident_dirs <- unlist(lapply(ident_stations, function(station) {
    cameras <- list.dirs(station, recursive = FALSE)
    unlist(lapply(cameras, function(camera) {
      list.dirs(camera, recursive = FALSE)
    }))
  }))
  
  # function to process species folders with identified images
  read_ident_folder <- function(ident_path) {
    img_files <- list.files(ident_path, pattern = "\\.jpe?g$", full.names = TRUE, ignore.case = TRUE)
    if (length(img_files) == 0) return(NULL)
    
    meta <- tryCatch(exif_read(img_files), error = function(e) NULL)
    if (is.null(meta)) return(NULL)
    
    # reduce to wanted headers
    known_fields <- c("FileName", "SourceFile", "Model", "DateTimeOriginal")
    meta <- meta[names(meta) %in% known_fields]
    
    meta$ct_station <- tolower(lapply(strsplit(as.character(ident_path),"/"), "[[", 3))
    meta$identified_as <- tolower(lapply(strsplit(as.character(ident_path),"/"), "[[", 5))
    
    as.data.frame(meta, stringsAsFactors = FALSE)
  }
  
  # start cluster
  cl <- parallel::makeCluster(num_cores)
  writeLines(paste0("\nCreating cluster for parallel processing, using ", num_cores, " of threads"))
  parallel::clusterEvalQ(cl, library(exiftoolr))
  parallel::clusterExport(cl, varlist = c("ident_dirs"))#, "raw_images"
  
  # read in meta data of all identified jpegs in parallel
  writeLines(paste0("\nProcessing of all identified images in parallel.\nThis might take a while, better go and get some coffee.\nPlease be patient and wait...."))
  meta_list <- parallel::parLapply(cl, ident_dirs, read_ident_folder)
  
  # stop cluster
  parallel::stopCluster(cl)
  
  # remove NULLs and combine results
  meta_list <- Filter(Negate(is.null), meta_list)
  ident_images <- do.call(rbind, meta_list)
  
  
  # start cluster and export objects and libs
  cl <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cl,"calc_hashes")
  # calculate hashes per identified folder in python
  hash_results <- parallel::parLapply(cl, ident_dirs, calc_hashes)
  parallel::stopCluster(cl)
  
  # collect results of nodes and polish path
  hash_df <- do.call(rbind, hash_results)
  hash_df$path = normalizePath(hash_df$path, winslash = "/", mustWork = FALSE)
  
  # adding hashes to raw_images
  ident_images = merge(ident_images, hash_df, by.x = "SourceFile", by.y = "path", all.x=T, all.y = F)
  
  # excluding obviously damaged images, where no hash could be generated
  no_hash_ident = ident_images[is.na(ident_images$sha256),]
  ident_images = ident_images[!(is.na(ident_images$sha256)),]
  
  
  
  # adding point_id. img_id
  merged_images <- merge(ident_images, raw_images[names(raw_images) %in% c("img_id", "sha256", "point_id", "proj_id")], by= "sha256", all.x=T, all.y=F)
  
  # process images without img_id
  no_match <- merged_images[is.na(merged_images$img_id), ]
  
  hamming_dist <- function(hash1, hash2) {
    sum(as.integer(intToBits(strtoi(substr(hash1, 1:nchar(hash1), 1:nchar(hash1)), 16))) !=
          as.integer(intToBits(strtoi(substr(hash2, 1:nchar(hash2), 1:nchar(hash2)), 16))))
  }
  
  # function to compute the hamming distance for every image without img_id
  match_one <- function(i) {
    ident_row <- no_match[i, ]
    
    distances <- mapply(function(r_ahash, r_dhash, r_phash) {
      mean(c(
        hamming_dist(ident_row$ahash, r_ahash),
        hamming_dist(ident_row$dhash, r_dhash),
        hamming_dist(ident_row$phash, r_phash)
      ))
    }, raw_images$ahash, raw_images$dhash, raw_images$phash)
    
    best_match_idx <- which.min(distances)
    
    if (distances[best_match_idx] <= 5) {
      return(list(
        matched_img_id = raw_images$img_id[best_match_idx],
        match_distance = distances[best_match_idx]
      ))
    } else {
      return(list(
        matched_img_id = NA_character_,
        match_distance = NA_real_
      ))
    }
  } # end of match_one function
  
  # parallel computing of hamming ditance
  cl <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cl, varlist = c("no_match", "raw_images", "hamming_dist"))
  parallel::clusterEvalQ(cl, library(parallel))
  
  matches <- parallel::parLapply(cl, 1:nrow(no_match, match_one))
  
  # collecting results from parallel nodes and merging them to no_match
  matched_img_id <- sapply(matches, `[[`, "matched_img_id")
  match_distance <- sapply(matches, `[[`, "match_distance")
  
  no_match$img_id <- matched_img_id
  
  no_match$comment <- paste0("img_id found by perceptual hashes with a mean hamming distance of ", match_distance) 
  
  no_match$proj_id = proj_id
  
  no_match <- merge(no_match[,!(names(no_match) %in% "point_id")], point_ids, by.x = "ct_station", by.y = "point_name", all.x =T, all.y = F)
  
  # adding img_ids found per perceptual hashes and comments to the rest
  merged_images$comment = NA
  
  merged_images = rbind(merged_images[!(merged_images$SourceFile %in% no_match$SourceFile),], no_match)
  
  names(merged_images) <- c("sha256", "file_path", "file_name", "cam_id", "date_original", "ct_station", "identified_as", "ahash", "dhash", "phash", "point_id", "proj_id", "img_id", "comment" )
  
  merged_images$point_id = as.numeric(merged_images$point_id)
  
  # write new images to DB
  writeLines("\nWriting identified images to database....")
  DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "ident_images"), merged_images, append = T)
  
  
  
  
  # adding ownership ----
  writeLines("\nWriting ownerships to database....")
  if(org_peo == "Organisations"){
    org_tab = merge(org_ownerships, raw_images[,"img_id"], by = NULL)
    names(org_tab)[names(org_tab) == "y"] = "img_id" 
    DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "img_owner_orgs"), org_tab, append = T)
    rm(org_tab)
  } #end of if clause 'Organisations'
  
  if(org_peo == "Persons"){
    pers_tab = merge(pers_ownerships, raw_images[,"img_id"], by = NULL)
    names(pers_tab)[names(pers_tab) == "y"] = "img_id" 
    DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "img_owner_pers"), pers_tab, append = T)
    rm(pers_tab)
  } #end of if clause 'Persons'
  
  
  
  
  # write ReadMe.txt file to input dir ----
  sink(paste0(in_dir,"/EcoDynDB_Import_",format(Sys.time(),"%Y%m%d_%H%M%S"), ".txt"))
  writeLines("[Header]")
  writeLines("Standardized camtrap data documentation file")
  writeLines("Created by R-function createImages() when camtrap images were added to the EcoDynDB")
  writeLines(paste0("Date:\t", as.character(Sys.Date())))
  writeLines(paste0("DB user:\t", DBI::dbGetInfo(db_con)$username))
  writeLines("\n")
  writeLines("[PROJECT INFO]")
  writeLines("Images were collected within the following project:")
  writeLines(paste0("EcoDynDB project name:\t", proj_name))
  writeLines(paste0("EcoDynDB project ID:\t", proj_id))
  writeLines(paste0("Start year of project:\t", proj_year))
  writeLines("\n")
  writeLines("[CONTENT]")
  writeLines(paste0("Number of raw images:\t", nrow(raw_images)))
  writeLines(paste0("Number of identified images:\t", nrow(ident_images)))
  writeLines(paste0("Damaged images, not imported:\t", nrow(no_hash_ident)))
  writeLines(paste0("Identified images without sha256 match:\t", nrow(no_match)))
  writeLines("\n")
  writeLines("[COPYRIGHTS]")
  writeLines("Organisations with copyright on the images:")
  if(exists("org_ownerships")){
    write.table(org_ownerships[,c(4,5,3)], append = T, row.names = F, col.names = F, sep = "; ", quote = FALSE)
    } else {writeLines("None")}
  writeLines("\n")
  writeLines("Persons with copyright on the images:")
  if(exists("pers_ownerships")){
    write.table(pers_ownerships[,c(4,5,3)], append = T, row.names = F, col.names = F, sep = "; ", quote = FALSE)
  } else {writeLines("None")}
  
  sink()
  
  # add info of input folder to database
  path_id <- DBI::dbGetQuery(db_con, paste0("SELECT path_id from camtrapdata.ct_folders where ct_folders.path = \'", in_dir,"\'  AND ct_folders.proj_id = ", proj_id))
  
  get_content <- function(path, script = NULL, python_bin = "python") {
    # Script-Pfad (Standard: aus deinem Paket holen)
    if (is.null(script)) {
      script <- system.file("python", "folder_size.py", package = "RHelperEcoDynDB")
               }
    if (script == "") stop("Python-script folder_size.py not found.")
    
    cmd <- c(script, shQuote(path))
    out <- system2(python_bin, args = cmd, stdout = TRUE, stderr = TRUE)
    
    # error handling, if Python prompted to stderr 
    if (length(attr(out, "status")) && !is.null(attr(out, "status"))) {
      stop("Python-script failed:\n", paste(out, collapse = "\n"))
    }
    
    # read csv output in dataframe
    df <- read.csv(text = out, stringsAsFactors = FALSE)
    return(df)
  }
  writeLines("\nCalculating size of folder contents. Please wait...")
  dir_content = get_content(in_dir)
  
  dir_content = dir_content[,c("name","type","size")]
  dir_content$path_id = path_id$path_id
  dir_content$size = as.numeric(dir_content$size)
  
  DBI::dbWriteTable(db_con, DBI::Id(schema = "camtrapdata", table = "ct_folders_content"), dir_content[,c("path_id","name","type","size")], append = T)
  
  
  # final messages ----
  writeLines(paste0("\nNumber of raw images written to the database: ", nrow(raw_images)))
  
  writeLines(paste0("\nNumber of identified images written to the database: ", nrow(ident_images)))
  
  writeLines("\nThanks for waiting so patiently.\nHave nice day!")
  
} #end of createImages() function

  
  
  