#' Read in data of sequencing runs from Illumina MiSeq runs
#' 
#' getSeqRun 
#' 
#' Function to read relevant data from Illumina MiSeq runfolders. The relevant data is written into the EcoDyn database.
#' 
#' @param in_dir Folder containing the PCR info xlsx-files. If not provided the user is requested select one.
#' 
#' @export

getSeqRun = function(in_dir = NA){
  
  # test for directory containing the sequencing run folders
  if(is.na(in_dir) == T){
    if(base::file.exists("T:/data_BioDiv/") == T){
      in_dir = utils::choose.dir(default = "T:/data_BioDiv/", "Select folder containing the PCR documentation files")
    } else {
      in_dir = utils::choose.dir(default = "Computer", "Select folder containing the PCR documentation files")
    }
  }
  
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  
  # select run folder
  folder = utils::choose.dir(default = in_dir, "Choose run folder")
  n_col = max(utils::count.fields(paste0(folder, "/SampleSheet.csv"), sep=","))
  sheet = utils::read.csv(paste0(folder, "/SampleSheet.csv"), header = F, col.names = paste0("V", seq_len(n_col)))
  l_no = as.numeric(row.names(sheet[grepl("\\[Data\\]", sheet$V1),]))
  pcr_batches = utils::read.csv(skip = l_no + 1, paste0(folder, "/SampleSheet.csv"), header = F, col.names = as.character(sheet[l_no+1,]))
  pcr_batches = pcr_batches[pcr_batches$Sample_ID != "",]
  
  info = XML::xmlRoot(XML::xmlParse(paste0(folder, "/RunInfo.xml")))
  run_name = basename(folder)
  run_date = as.Date(XML::xpathSApply(info, '//Date', XML::xmlValue), format = "%y%m%d")
  seq_machine = XML::xpathSApply(info, '//Instrument', XML::xmlValue)
  x = data.frame(t(XML::xpathSApply(info, "///Read", XML::xmlToList)))
  read1_cycles = x$NumCycles[x$Number == 1]
  read1_index = x$IsIndexedRead[x$Number == 1]
  read2_cycles = x$NumCycles[x$Number == 2]
  read2_index = x$IsIndexedRead[x$Number == 2]
  read3_cycles = x$NumCycles[x$Number == 3]
  read3_index = x$IsIndexedRead[x$Number == 3]
  read4_cycles = x$NumCycles[x$Number == 4]
  read4_index = x$IsIndexedRead[x$Number == 4]
  
  # create data frame from selected information
  new_run = data.frame(run_name, run_date, seq_platform = "Illumina Miseq", seq_machine, read1_cycles, read1_index, read2_cycles, read2_index, read3_cycles, read3_index, read4_cycles, read4_index)
  
  # query database if seq run already exists
  db_runs <- DBI::dbReadTable(db_con, DBI::Id(schema = "sfb", table = "seq_runs"))
  if(new_run$run_name %in% db_runs$run_name){
    run_exist = db_runs[db_runs$run_name == new_run$run_name,]
    stop(paste0("There is already an existing sequencing run in the database with the same name:\n", "run name:\t", run_exist$run_name,"\n", "run date:\t",run_exist$run_date,"\n", "run folder:\t", run_exist$run_folder, "\nThis process is aborted. Please rename current run!"))
  } else {
    # write to database  
    DBI::dbWriteTable(db_con, DBI::Id(schema = "sfb", table = "seq_runs"), new_run, append = T)
  }
  
  
  # create tax_query string q_1 for more flexible LIKE ANY query in combination with ARRAY
  for(i in 1 : length(pcr_batches$Sample_ID)){
    q_i = paste0("'", paste0("%", pcr_batches$Sample_ID[i], "%"), "'")
    if(i == 1){
      q_1 = q_i
    }
    if(i > 1){
      q_1 = paste(q_1, q_i, sep = ", ")
    }
  }
  
  # database query
  seq_samples = DBI::dbGetQuery(db_con, paste0(
    "SELECT 
       sfb.plate_samples.plate_name,
       extr_name,
       i2 AS i2_1,
       i2 AS i2_2,
       i1 AS i1_1,
       i1 AS i1_2
     FROM
	     sfb.plate_samples
     LEFT JOIN 
       sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.plate_samples.plate_name
     WHERE
       sfb.pcr_plates.plate_name LIKE ANY (ARRAY[", q_1, "])
    "
  ))
  
  seq_samples$run_name = run_name
  #DBI::dbWriteTable(db_con, DBI::Id(schema = "sfb", table = "seq_samples"), seq_samples, append = T)
  
  
  # end database connection
  if(exists("conn_test") == TRUE){
    EcoDynDisconnect()
  }
  
  
  
}