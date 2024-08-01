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
                              pcr_batches = NULL
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
  # Function to check if all values in a vector are integers
  check_integers <- function(x) {
    all(sapply(x, function(y) all.equal(y, as.integer(y)) == TRUE))
  }
  if(is.null(pcr_batches) == T){
    stop("No PCR batches provided.\nPlease define, e.g. as \'pcr_batches =c(1:5)\' or \'pcr_batches =c(1, 25, 305)\'")
  }
  if(check_integers(pcr_batches) == F){
    stop("\'pcr_batches\' was not provided as integer.\nPlease define, e.g. as \'pcr_batches =c(1:5)\' or \'pcr_batches =c(1, 25, 305)\'")
  }
  pcr_batches = sprintf("p%03d", pcr_batches)
  
  
  # test for in_dir and select quantification files
  if(is.na(in_dir) == T){
    if(file.exists("D:/BioDivCloud/1_06_data_progress/VN_SaolaLab_2024/quantification") == T){
      in_dir = utils::choose.dir(default = "D:/BioDivCloud/1_06_data_progress/VN_SaolaLab_2024/quantification", "Select folder containing the PCR documentation files")
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
  
  # query data on pcr batch and dna templates from EcoDyn database 
  tab = data.frame(pcr_id = numeric(0), plate_name = character(0), extract_id = numeric(0),  extr_name = character(0))
  for(batch in pcr_batches){
    writeLines(paste0("\nQuery data from EcoDynDB for ", batch))
    tab_q = DBI::dbGetQuery(
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
         sfb.pcrs.plate_name LIKE '", batch, "'
         AND
         sfb.pcrs.pcr_no LIKE '2nd'
         ORDER BY
         sfb.pcrs.plate_name, extr_name"
        )
      )
    cont_add = tab_q[1:2,]
    cont_add$extract_id = NA
    cont_add$extr_name[1] = paste0(cont_add$plate_name[1],"_pos_contr")
    cont_add$extr_name[2] = paste0(cont_add$plate_name[2],"_neg_contr")
    if(nrow(tab_q) < 24){
      tab_q = rbind(tab_q, matrix(data = NA, nrow = 24-nrow(tab_q), ncol = 4, byrow = FALSE, dimnames = list(c(1:(24-nrow(tab_q))), names(tab_q))))
    }
    tab_q = rbind(tab_q, cont_add)
    writeLines(paste0("\nFound ", nrow(tab_q), " entries for ", batch))
    tab <<- rbind(tab, tab_q) 
    writeLines(paste0("\n", nrow(tab), " entries now combined in tab"))
  }
 
  
  
  
  
  # get data from quantification files
  measures = data.frame(molarity = numeric(0), concentration = numeric(0), date_measure = character(0))
  
  for(file in files){
    writeLines(paste0("\nProcessing file ", file))
    f_path = source_files[grepl(file, source_files)]
    quant_file = readxl::read_xls(f_path)
    
    for(x in 2: (length(pcr_batches)*4 + 1)){
      print(x)
      if(x %in% c(5,9,13,17,21)){
        for(y in  seq(from = 1, to = 3, by = 2)){
          val1 = as.numeric(quant_file[y,x])
          val2 = as.numeric(quant_file[y+1,x])
          if(is.na(val1) | is.na(val2)){
            if(is.na(val1)){val1=0} 
            if(is.na(val2)){val2=0} 
            concentration = (val1 + val2)
            molarity = concentration * 5.64
          }
          concentration = (val1 + val2)/2
          molarity = concentration * 5.64
          date_measure = gsub(".xls", "", tail(strsplit(basename(file), "_")[[1]],1))
          measures <<- rbind(measures, cbind(molarity, concentration, date_measure))
        }
      } else{
        for(y in  seq(from = 1, to = 15, by = 2)){
        
        val1 = as.numeric(quant_file[y,x])
        val2 = as.numeric(quant_file[y+1,x])
        if(is.na(val1) | is.na(val2)){
          if(is.na(val1)){val1=0} 
          if(is.na(val2)){val2=0} 
          concentration = (val1 + val2)
          molarity = concentration * 5.64
        }
        concentration = (val1 + val2)/2
        molarity = concentration * 5.64
        date_measure = gsub(".xls", "", tail(strsplit(basename(file), "_")[[1]],1))
        measures <<- rbind(measures, cbind(molarity, concentration, date_measure))
      }
      }
      
      
    }
  }
  
  if(nrow(measures) != nrow(tab)){
    stop(paste0("Measurements (",nrow(measures),") and samples (", nrow(tab), ") don't match!"))
  }
  writeLines(paste0("\nPutting things together...."))
  
  tab = cbind(tab, measures[1:nrow(tab),])
  tab$date_measure = as.Date(tab$date_measure, "%Y%m%d")
  tab$molarity[is.na(tab$molarity)] = 0
  tab$concentration[is.na(tab$concentration)] = 0
  tab = tab[!(is.na(tab$pcr_id)),]
 
  
  DBI::dbWriteTable(db_con, DBI::Id(schema="sfb", table="molarities"), tab, append = T)
  
  if(exists("conn_test") == TRUE){
    EcoDynDisconnect()
  }
  }

  