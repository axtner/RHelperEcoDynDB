#' Read in data of PCRs from standardized documentation files
#' 
#' get_pcr 
#' 
#' Function to read standardized PCR info xlsx-files. The relevant data is written into the EcoDyn database and some tables are updated.
#' 
#' @param in_dir Folder containing the PCR info xlsx-files. If not provided the user is requested select one.
#' 
#' @export

getPcrInfo= function(in_dir = NA){
  
  # test for directory containing the PCR documentation files
  if(is.na(in_dir) == T){
    if(file.exists("T:/data_BioDiv/") == T){
      in_dir = utils::choose.dir(default = "T:/data_BioDiv/", "Select folder containing the PCR documentation files")
    } else {
      in_dir = utils::choose.dir(default = "Computer", "Select folder containing the PCR documentation files")
        }
  }
  
  in_dir = gsub("\\\\", "/", in_dir)
  source_files = list.files(in_dir, full.names = T)
  source_files = source_files[grepl("p*.xlsx", source_files)]
  source_files = source_files[order(source_files, decreasing = T)]
  
  sel_files = utils::select.list(basename(source_files), title = "Please select PCR files:", multiple = T, graphics = T)
  if(identical(sel_files, character(0)) == T){
    stop("No PCR file chosen. Process aborted")
  }
  files = source_files[basename(source_files) %in% sel_files]
  i1 = c(LETTERS[c(1:9, 11:20, 22:26)], "ctr")
  
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # loop through selected PCR info files
  for(file in files){
    f_path <- source_files[grepl(file, source_files)]
    pcr_file <- readxl::read_xlsx(f_path)
    plate_no <- as.character(pcr_file[1,3])
    sample_no <- as.character(pcr_file[2,3])
    samples <- pcr_file[7:30,3][!(is.na(pcr_file[7:30,3]))]
    
    new_pcrs <- data.frame(pcr_date = pcr_file[c(2, 19),6], plate_id = as.character(plate_no), pcr_no = c("1st", "2nd"), rxn_vol = pcr_file[c(4, 22), 6], template_vol = pcr_file[c(16, 34), 6], gen_marker = pcr_file[4, 3])
    names(new_pcrs) <- c("pcr_date", "plate_name", "pcr_no", "rxn_vol", "template_vol", "gen_marker")
    
    
    t1 <- DBI::dbAppendTable(db_con, DBI::Id(schema="sfb", table="pcr_plates"), data.frame(plate_name = plate_no, i2 = as.character(pcr_file[20,6])))
    
    t2 <- DBI::dbAppendTable(db_con, DBI::Id(schema="sfb", table="plate_samples"), data.frame(extr_name = samples, plate_name = as.character(plate_no), i1 = i1[1:length(samples)]))
    
    v1 <- DBI::dbGetQuery(db_con, "SELECT plate_name FROM sfb.pcr_plates")
    
    while((unique(new_pcrs$plate_name) %in% v1$plate_name) == FALSE){
      v1 <- DBI::dbGetQuery(db_con, "SELECT plate_name FROM sfb.pcr_plates")
    }
    
    t3 <- DBI::dbAppendTable(db_con, DBI::Id(schema = "sfb", table = "pcrs"), new_pcrs)
    
    
    
    # Create a new temporary table in the database to hold the latest data
    temp_table <- DBI::dbSendStatement(db_con, 
                                       "CREATE TEMPORARY TABLE temp_latest_data AS
                                        SELECT
                                        extr_name,
                                        COUNT(*) AS number_of_pcrs,
                                        ARRAY_AGG(CONCAT(pcr_plates.i2, sfb.plate_samples.i1)) AS i2_i1_combinations
                                        FROM
                                        sfb.plate_samples
                                        LEFT JOIN
                                        sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.plate_samples.plate_name
                                        GROUP BY
                                        extr_name"
                                       )
    DBI::dbClearResult(temp_table)
    
    # Update the main table with the latest data from the temporary table in the database
    insert <- DBI::dbSendStatement(db_con, 
                                   "INSERT INTO sfb.number_of_pcrs (extr_name, number_of_pcrs, i2_i1_combinations)
                                    SELECT
                                    extr_name,
                                    number_of_pcrs,
                                    i2_i1_combinations
                                    FROM
                                    temp_latest_data
                                    ON CONFLICT (extr_name) DO UPDATE
                                    SET
                                    number_of_pcrs = EXCLUDED.number_of_pcrs,
                                    i2_i1_combinations = EXCLUDED.i2_i1_combinations"
                                   )
    DBI::dbClearResult(insert)
    
    # Drop the temporary table from the database
    drop_tab <- DBI::dbSendStatement(db_con, "DROP TABLE IF EXISTS temp_latest_data")
    DBI::dbClearResult(drop_tab)
  }
  
  writeLines(paste0("\nWrote data of ", paste(data.frame(plate_name = plate_no, i2 = as.character(pcr_file[20,6])), collapse = " (i2: ") , ") to the EcoDyn database.\n"))
  
  if(exists("conn_test") == TRUE){
    EcoDynDisconnect()
  }
  
}