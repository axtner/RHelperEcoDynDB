#' add geometries of points (e.g. camera-trapping stations)
#' 
#' createPoints 
#' 
#' Function to read in geometries of points from tables.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

createPoints <- function(in_file = NA, ct_table=NA){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # welcome message
  writeLines("\nWelcome!\nYou want to point geometries to the EcoDyn database. Point names and point geometries are stored independently, each with an Unique ID. Thus the coordinates of a point can have more than one point name, which can be linked with different projects.\n1.You will have to chose the file from where to import the point names and their geometries from. Make sure the header of the document is just a single line.  \n2.You can link those site directly with an existing EcoDyn project, if you like.\n")
  
  
  # 1. select input data ----
  in_dir <<- utils::choose.dir(default = "Computer", "Select directory that contains the input file")
  
  if(exists("in_dir")){
    file_name <- utils::select.list(list.files(in_dir, pattern ="xlxs|csv|txt|xls"), title = "Select input file", graphics=F)
    file_path <- list.files(in_dir, full.names = T, pattern = file_name)#[grepl(in_file, list.files(in_dir, full.names = T))]
    file_path <- gsub("\\\\", "/", file_path)
    if(length(file_path) != 1){
      file_path <- utils::select.list(file_path, title = "Sorry, please select the correct file:")
    }
  }
  
  # read in EXEL spreadsheets
  if(grepl(".xls", file_path)){
    writeLines("Please choose the Excel sheet containing the point geometries.")
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
  
  writeLines(paste0("The loaded table has ", nrow(ct_table), " lines and looks like the following:"))
  print(ct_table[1:5,])
  continue <- utils::select.list(c("Yes", "No"), title = "Is everything correct?", graphics=F)
  if(continue == "No"){stop("Something is wrong with the input file, please start again")}
    
  # point names
  pointnames <- utils::select.list(names(ct_table), title = "Select point name column", graphics=F)
  
  # geometries
  long <- utils::select.list(names(ct_table), title = "Select longitude column", graphics=F)
  lat <- utils::select.list(names(ct_table), title = "Select latitude column", graphics=F)
  
  if(nrow(ct_table[is.na(ct_table[[long]]) | is.na(ct_table[[lat]]),]) > 0){
    message(paste0("\nThere were ", nrow(ct_table[is.na(ct_table[[long]]) | is.na(ct_table[[lat]]),]), " stations without full coordinates. These stations were excluded, please check!"))
    ct_table <- ct_table[!(is.na(ct_table[[long]]) | is.na(ct_table[[lat]])),]
    }
  point_name <- tolower(ct_table[[pointnames]])
  
  long_val <- as.numeric(ct_table[[long]])
  lat_val <- as.numeric(ct_table[[lat]])
  
  # create wkt geometries
  geom <- wk::wkt(paste0("POINT(", long_val, " ", lat_val, ")"))
  
  crs <- utils::select.list(c("WGS84", "UTM"), title = "Are these WGS84 or UTM coordinates?", graphics=F)
  
  if(crs == "UTM"){
    continue = "No"
    find_crs = function(){
    zone <- readline("Enter the UTM zone (e.g. 48): ")
    sel_crs <<- DBI::dbGetQuery(db_con, paste0("SELECT proj4text, auth_name, srid
                                               FROM spatial_ref_sys
                                               WHERE auth_name = 'EPSG'
                                               AND auth_srid = 326", zone, ";"))
    writeLines(paste0("\nSuggested CRS :", sel_crs$proj4text))
    continue <<- utils::select.list(c("Yes", "No"), title = "\nIs the suggested CRS correct?", graphics=F)
    if(continue == "No"){
      writeLines("Do you know the correct CRS and want to change it, or do you want to exit?")
      clue <-  utils::select.list(c("Yes, I know it, let me change it", "I have no clue, let me exit and get more info"), graphics=F)
      if(clue == "I have no clue, let me exit and get more info"){
        stop("\nFor now you will exit. Little homework for you: find out the correct CRS.")
        }
      }
    }
    while(continue == "No"){find_crs()}
    crs_id <<- sel_crs$srid
  }
  
    
  # dates geometries were taken
  date_col <- utils::select.list(c(names(ct_table), NA), title = "Select date column, if available", graphics=F)
  if(!(is.na(date_col))){
    ct_table[[date_col]] <- gsub("/", "-", ct_table[[date_col]])
    writeLines("Present date formats:")
    print(unique(ct_table[,names(ct_table) == date_col]))
    writeLines("\n")
    date_form <- utils::select.list(c("2025-05-30", "30-05-2025", "05-30-2025", "Friday, 30 May 2025", "diff. formats, unknown"), title = "What date format is used?", graphics=F)
    dates <- ct_table[[date_col]]
    
    
    if(date_form == "diff. formats, unknown"){
      stop("\nPlease find out or contact the admin.")
    }
    if(date_form == "2025-05-30"){
      gps_time <- as.Date(dates, format = "%Y-%m-%d")
    }
    if(date_form == "30-05-2025"){
      gps_time <- as.Date(dates, format = "%d-%m-%Y")
    }
    if(date_form == "05-30-2025"){
      gps_time <- as.Date(dates, format = "%m-%d-%Y")
    }
    
    if(date_form == "Friday, 30 May 2025"){
      gps_time <- lubridate::parse_date_time(
        dates,
        orders = c("Y-m-d H:M", "Y-m-d", "d B Y"),
        locale = "C"
      )
      }
   } else {
    gps_time = rep(NA, nrow(ct_table))
    }
  
  
  # GPS point accurancy
  point_accuracy <<- utils::select.list(c(names(ct_table), NA), title = "Select point accuracy column, if available", graphics=F)
  if(is.na(point_accuracy) == F){
    point_accuracy <<- ct_table[[point_accuracy]]
  } else {
    point_accuracy = rep(NA, nrow(ct_table))
    }
    
  
  # gps-measured elevation
  gps_elevation <<- utils::select.list(c(names(ct_table), NA), title = "Select elevation column, if available", graphics=F)
  if(is.na(gps_elevation) == F){
    gps_elevation <<- ct_table[[gps_elevation]]
  } else {
    gps_elevation = rep(NA, nrow(ct_table))
    }
      
  # query available countries
  countries <<- DBI::dbGetQuery(db_con, "SELECT country_name_short FROM geodata.countries ORDER BY country_name_short")
  c_filter = readline("Enter country name (partly or first letter) to restrict search: ")
  country <<- utils::select.list(countries$country_name_short[grepl(tolower(c_filter), tolower(countries$country_name_short))], title = "Select country", graphics=F)
  country = rep(country, nrow(ct_table))
    
  # query available sites
  sites <<- DBI::dbGetQuery(db_con, "SELECT DISTINCT site FROM geodata.point_names ORDER BY site")
  s_filter = readline("Enter site name (partly or first letter) to restrict search: ")
  site <<- utils::select.list(c(sites$site[grepl(tolower(s_filter), tolower(sites$site))], "Other"), title = "Select study site", graphics=F)
  if(site == "Other"){
    site <<- readline("Enter site name: ")
  }
  site = rep(site, nrow(ct_table))
  
  
  
  # 2. select project ----
  sel_proj = function(){
    writeLines("\nDo you want to link those points to an existing project of the EcoDyn database?")
    link_points <<- utils::select.list(c("Yes", "No"), graphics = F)
    if(link_points == "Yes"){
      projects <<- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
      project <<- utils::select.list(c(paste(projects$proj_name, projects$proj_year), "Other"),
                                     title = "Select a project by choosing it's according number or '0' for exiting:", 
                                     graphics = FALSE)
      if(project == "Other" | project == ""){
        message("\nSorry, that you couldn't find your project in the EcoDyn database.\nYou can stop now and add the project by using the createProject() function of the RHelperEcoDynDB package, or you can continue without linking those points to a project.")
        continue <<- utils::select.list(c("Yes", "No"), title = "Do you want to continue without project?", graphics = F)
        if(continue =="No"){stop("\nYou decided to exit. Have a nice day!")} else {link_points <<- "No"}
      } else {
        project <<- projects[paste(projects$proj_name, projects$proj_year) == project,]
        proj_name <<- project$proj_name
        proj_id <<- project$proj_id
        }
      }
    if(link_points == "No"){project <<- NA}
  }
  sel_proj()
  
  
  # create dataframe
  df_imp <<- data.frame(point_name, geom, point_accuracy, gps_time, gps_elevation, country, site)
  
  
    
  writeLines("\nThe data you are about to upload looks like the following:")
  print(df_imp[1:5,])
  continue <- utils::select.list(c("Yes", "No"), title = "Is everything correct?", graphics=F)
  if(continue == "No"){stop("Something is wrong with the input file, please start again")}
  
  # test if site point names are unique or do already exist in database
  DBI::dbWriteTable(db_con, "temp_new_points", df_imp, temporary = TRUE, overwrite = TRUE) 
  
  # convert UTM in WGS84
  if(crs == "UTM"){
    DBI::dbExecute(db_con, paste0("UPDATE temp_new_points SET geom = ST_AsText(ST_Transform(ST_SetSRID(ST_GeomFromText(geom), ", crs_id,"), 4326));"))
  }
  
  # test if points of the data set share coordinates
  duplicates <- DBI::dbGetQuery(db_con, "WITH duplicates AS (
                                                             SELECT a.point_name,
                                                                    a.geom,
                                                                    ROW_NUMBER() OVER (
                                                                                       PARTITION BY ST_AsText(a.geom)
                                                                                       ORDER BY point_name  -- definies the first
                                                                                       ) AS rn
                                                             FROM temp_new_points a
                                                             WHERE EXISTS (
                                                                           SELECT 1
                                                                           FROM temp_new_points b
                                                                           WHERE a.ctid <> b.ctid
                                                                           AND ST_DWithin(a.geom::geography, b.geom::geography, 1)
                                                                           )
                                                             )
                                           SELECT point_name, 
                                                  geom
                                           FROM duplicates
                                           WHERE rn > 1  -- all except the first
                                           ORDER BY geom, point_name;"
                                )
  
   # test if geometries already exist in database
   unique_geo <- DBI::dbGetQuery(db_con, "SELECT n.point_name,
                                                  geom,
                                                  point_accuracy,
                                                  gps_time, 
                                                  gps_elevation
                                            FROM temp_new_points n 
                                            WHERE NOT EXISTS (
                                                              SELECT 1 
                                                              FROM geodata.point_geometries p 
                                                              WHERE ST_DWithin(
                                                                               n.geom::geography, 
                                                                               p.geom::geography, 
                                                                               1  -- Tolerance in meter
                                                                               )
                                                              );"
    ) 
   
   # exclude duplicates from the unique points list
   if(nrow(duplicates) > 0){
     unique_geo <<- unique_geo[!paste(unique_geo$point_name, unique_geo$geom) %in% paste(duplicates$point_name, duplicates$geom), ]
     }
  
  
   # write new points into geodata.point_geometries and query the new p_geo_id
   DBI::dbWriteTable(db_con, DBI::Id(schema="geodata", table="point_geometries"), unique_geo[,c(2:5)], append = T)
   
   db_geom <<- DBI::dbGetQuery(db_con, "SELECT p_geo_id, ST_AsText(geom) AS geom 
                                          FROM geodata.point_geometries;"
                               )
   
   # add p_geo_id to the point names
   new_point_names <<- DBI::dbGetQuery(db_con, "SELECT DISTINCT n.point_name,
                                                                pg.p_geo_id,
                                                                n.gps_time AS p_date,
                                                                n.site,
                                                                n.country
                                                FROM temp_new_points n
                                                JOIN geodata.point_geometries pg ON ST_DWithin(
                                                                                               n.geom::geography, 
                                                                                               pg.geom::geography, 
                                                                                               1  -- Tolerance in meter
                                                                                               );"
                                       )
   
   # add only new point_names to table geodata.point_names
   stmt <- DBI::dbSendStatement(db_con, "INSERT INTO geodata.point_names (point_name, p_geo_id, p_date, site, country)
                                 VALUES ($1, $2, $3, $4, $5)
                                 ON CONFLICT(point_name, p_geo_id, p_date, site) DO NOTHING;")
   DBI::dbBind(stmt, params = list(
     new_point_names$point_name,
     new_point_names$p_geo_id,
     new_point_names$p_date,
     new_point_names$site,
     new_point_names$country
   ))
   
   # query p_name_ids
   db_names <<- DBI::dbGetQuery(db_con, "SELECT * FROM geodata.point_names;")
    
   
   
   # link new points to project
   if(link_points == "Yes"){
     # query p_name_id from database
     p_name_ids <- DBI::dbGetQuery(db_con, "SELECT pn.p_name_id
                                            FROM temp_new_points n
                                            LEFT JOIN geodata.point_names pn ON n.point_name = pn.point_name
                                            AND n.gps_time = pn.p_date AND n.site = pn.site AND n.country = pn.country;"
                                   )
     p_name_ids$proj_id = proj_id
      
     DBI::dbWriteTable(db_con, DBI::Id(schema="projects", table="proj_sites"), p_name_ids, append = T)
     }
}
  
  
  
  
  
  
  
 
  