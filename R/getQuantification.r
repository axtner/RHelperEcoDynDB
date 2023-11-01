#' Read in data of PCRs from standardized quantification xls-files exported from a TECAN plate reader
#' 
#' getQuantification 
#' 
#' Function to read standardized quantification xls-files. The relevant data is written into the EcoDyn database and some tables are updated.
#' 
#' @param in_dir Folder containing the PCR info xls-files. If not provided the user is requested select one.
#' 
#' @param pcr_batches Mandatory integers that characterizing the PCR batches that were quantified in the respective xls-files. For example "pcr_batches =c(1:5)" will query the database for the samples of the PCR batches "p001", "p002", "p003", "p004" and "p005", "pcr_batches =c(1, 25, 305)" will query for the PCR batches "p001", "p025" and "p305". 
#' 
#' @export

getQuantification = function(in_dir = NA,
                              pcr_batches = NA
                              ){
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # test pcr_batches and format them for SQL query
  if(is.na(pcr_batches) == T){
    stop("No PCR batches provided.\nPlease define, e.g. as \'pcr_batches =c(1:5)\' or \'pcr_batches =c(1, 25, 305)\'")
  }
  if(is.integer(pcr_batches) == F){
    stop("\'pcr_batches\' was not provided as integer.\nPlease define, e.g. as \'pcr_batches =c(1:5)\' or \'pcr_batches =c(1, 25, 305)\'")
  }
  insert_1 = sprintf("p%03d", pcr_batches)
  
  
  # query data on pcr batch and dna templates from EcoDyn database  
  tab = DBI::dbGetQuery(
    db_con,
    paste0(
      "SELECT
        sfb.pcrs.pcr_id,
        sfb.pcrs.plate_name,
        nuc_acids.extractions.extract_id AS extract_id,
        sfb.plate_samples.extr_name
       FROM
        sfb.plate_samples
       LEFT JOIN 
        sfb.pcrs ON sfb.plate_samples.plate_name = sfb.pcrs.plate_name
       LEFT JOIN 
        nuc_acids.extractions ON sfb.plate_samples.extr_name = nuc_acids.extractions.extr_name 
       WHERE 
        sfb.pcrs.plate_name LIKE ANY (ARRAY [\'%",paste(insert_1, collapse="%\',\'%"),"%\'])
       AND
        sfb.pcrs.pcr_no LIKE '2nd'
       ORDER BY
        sfb.pcrs.plate_name, extr_name
      "
    )
  )
  
  
  # test for in_dir and select quantification files
  if(is.na(in_dir) == T){
    if(file.exists("T:/data_BioDiv/") == T){
      in_dir = utils::choose.dir(default = "T:/data_BioDiv/", "Select folder containing the PCR documentation files")
    } else {
      in_dir = utils::choose.dir(default = "Computer", "Select folder containing the PCR documentation files")
    }
  }
  in_dir = gsub("\\\\", "/", in_dir)
  
  source_files = list.files(in_dir, full.names = T)
  source_files = source_files[grepl(".xls", source_files)]
  source_files = source_files[order(source_files, decreasing = F)]
  sel_files = utils::select.list(basename(source_files), title = "Please select quantification files:", multiple = T, graphics = T)
  if(identical(sel_files, character(0)) == T){
    stop("No quantification file chosen. Process aborted")
  }
  files = source_files[basename(source_files) %in% sel_files]
  
  
  # get data from quantification files
  measures = data.frame(molarity = numeric(0), concentration = numeric(0), date_measure = character(0))
  
  for(file in files){
    f_path = source_files[grepl(file, source_files)]
    quant_file = readxl::read_xls(f_path)
    
    for(x in 2:22){
      for(y in  seq(from = 1, to = 15, by = 2)){
        concentration = (as.numeric(quant_file[y,x]) + as.numeric(quant_file[y+1,x]))/2
        molarity = concentration * 5.64
        date_measure = gsub(".xls", "", strsplit(basename(file), "_")[[1]][4])
        measures <<- rbind(measures, cbind(molarity, concentration, date_measure))
      }
    }
  }
  
  if(nrow(measures) < nrow(tab)){
    stop(paste0("You have less measurements (",nrow(measures),") than samples (", nrow(tab), ")!"))
  }
  
  tab = cbind(tab, measures[1:nrow(tab),])
  tab$date_measure = as.Date(tab$date_measure, "%Y%m%d")
  tab$molarity[is.na(tab$molarity)] = 0
  tab$concentration[is.na(tab$concentration)] = 0
  
  DBI::dbWriteTable(db_con, DBI::Id(schema="sfb", table="molarities"), tab, append = T)
  
  if(exists("conn_test") == TRUE){
    EcoDynDisconnect()
  }
  }

  