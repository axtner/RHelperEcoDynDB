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
  
  # exit function  
  stop_quietly <<- function() {
    opt <- options(show.error.messages = F)
    on.exit(options(opt))
    if(exists("conn_test") == TRUE){
      EcoDynDisconnect()
    }
    message("\nYou decided to stop the process without saving. Have a nice day!")
    stop()
  }
  
  # test for directory containing the sequencing run folders
  folder = if(is.na(in_dir) == T){
    if(base::file.exists("T:/data_BioDiv/") == T){
      in_dir = utils::choose.dir(default = "T:/data_BioDiv/", "Select the run folder")
    } else {
      in_dir = utils::choose.dir(default = "Computer", "Select the run folder")
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
  

  # query database if seq run already exists
  name_funct <- function(){
    db_runs <<- DBI::dbReadTable(db_con, DBI::Id(schema = "sfb", table = "seq_runs"))
    last_runs <<- db_runs[order(as.Date(db_runs$run_date, format = "%Y-%m-%d"), decreasing = T), c(2,3,16,15)]
    writeLines("\nThe last five sequencing runs documented in the EcoDyn DB were the following:")
    print(last_runs[1:5,])
    # chose common name for run
    run_name <<- readline("Please chose a run name:")
  }
  name_funct()
  
  
  # read in parameters from SampleSheet.csv
  if(file.exists(paste0(folder, "/SampleSheet.csv"))) {
     n_col = max(utils::count.fields(paste0(folder, "/SampleSheet.csv"), sep=","))
     sheet = utils::read.csv(paste0(folder, "/SampleSheet.csv"), header = F, col.names = paste0("V", seq_len(n_col)))
     l_no = as.numeric(row.names(sheet[grepl("\\[Data\\]", sheet$V1),]))
     pcr_batches = sheet[c((l_no + 2) : nrow(sheet)),]
     names(pcr_batches) = sheet[l_no +1,]
     #pcr_batches = utils::read.csv(skip = l_no + 1, paste0(folder, "/SampleSheet.csv"), header = F, col.names = as.character(sheet[l_no+1,]))
     pcr_batches = pcr_batches[pcr_batches$Sample_ID != "",]
     } else {
     stop(paste0("\nMissing SampleSheet.csv in run folder '", folder, "'."))
     }
 
  
  # read in parameters from RunInfo.xml
  if(file.exists(paste0(folder, "/RunInfo.xml"))) {
    info <- XML::xmlParse(paste0(folder, "/RunInfo.xml"))
    seq_run_id =  XML::xmlGetAttr(XML::xmlRoot(info)[[1]], "Id")
    run_date = as.Date(XML::xpathSApply(info, '//Date', XML::xmlValue), format = "%y%m%d")
    seq_machine = XML::xpathSApply(info, '//Instrument', XML::xmlValue)
    x = data.frame(t(XML::xpathSApply(info, "///Read", XML::xmlToList)))
    if(nrow(x) == 4){
      read1_cycles = x$NumCycles[x$Number == 1]
      read1_index = x$IsIndexedRead[x$Number == 1]
      read2_cycles = x$NumCycles[x$Number == 2]
      read2_index = x$IsIndexedRead[x$Number == 2]
      read3_cycles = x$NumCycles[x$Number == 3]
      read3_index = x$IsIndexedRead[x$Number == 3]
      read4_cycles = x$NumCycles[x$Number == 4]
      read4_index = x$IsIndexedRead[x$Number == 4]
      
    }
    if(nrow(x) == 2){
      read1_cycles = x$NumCycles[x$Number == 1]
      read1_index = x$IsIndexedRead[x$Number == 1]
      read2_cycles = x$NumCycles[x$Number == 2]
      read2_index = x$IsIndexedRead[x$Number == 2]
      read3_cycles = NA
      read3_index = NA
      read4_cycles = NA
      read4_index = NA
    }
  } else {
    stop(paste0("\nMissing RunInfo.xml in run folder '", folder, "'.\nProcess exited."))
  }
    
 
  # read in parameters from RunCompleteionStatus.xml 
  if (file.exists(paste0(folder, "/RunCompletionStatus.xml"))) {
    completed <- XML::xmlParse(paste0(folder, "/RunCompletionStatus.xml"))
    compl_reads <<- as.numeric(XML::xpathSApply(completed, '//CycleCompleted', XML::xmlValue))
    total_reads <<- as.numeric(XML::xpathSApply(completed, '//TotalCycles', XML::xmlValue))
    status <<- "completed"
    if(compl_reads < total_reads){
      message("\nThe run has not completed all read cycles! Please check!\n")
    }
    run_error <<- XML::xpathSApply(completed, '//ErrorDescription', XML::xmlValue)
    if(run_error != "None"){
      message(paste0("The following run error is documented for this run:\n'",
      run_error,"'"))
      status <<- utils::select.list(c("failed", 
                                  "completed",
                                  "let me exit and check"
                                  ), title = "\nHas the run failed or is it complete?", graphics = FALSE)
      if(grepl("check", status)){
        stop_quietly()
      }
    }
  } else {
    message(paste0("There is no RunCompletionStatus.xml in run folder '", folder, "'.\nTherefore the status of the run could not be determined.\nPlease set the status manually."))
    status <<- utils::select.list(c("failed", 
                                "completed",
                                "let me exit and check"
    ), title = "\nHas the run failed or is it complete?", graphics = FALSE)
    if(grepl("check", status)){
      stop_quietly()
    }
  }
  # create data frame from selected information
  new_run <<- data.frame(run_name, run_date, seq_platform = "Illumina Miseq", seq_machine, read1_cycles, read1_index, read2_cycles, read2_index, read3_cycles, read3_index, read4_cycles, read4_index, seq_run_id, status, created_by = DBI::dbGetInfo(db_con)$username)
  
  
  
  #check if run exists, if not write run to sfb.seq_runs table in DB
  if(run_name %in% db_runs$run_name){
    run_exist <<- db_runs[db_runs$run_name == run_name, c(2,3,16,15)]
    writeLines("\nThe following existing runs of the database share the same run name:")
    print(run_exist)
    identical <- run_exist[run_exist$run_name == new_run$run_name & 
                           run_exist$run_date == new_run$run_date &
                           run_exist$status == new_run$status &
                           run_exist$seq_run_id == new_run$seq_run_id,]
    if(nrow(identical) > 0){
      message(paste0("\nThis run already exists in the database:\n", "run name:\t", identical$run_name,"\nrun date:\t", identical$run_date,"\nstatus:\t\t", identical$status,"\nrun id:\t\t", identical$seq_run_id,"\n\nThus this run is not written to the database and the process is exited!\n"))
      EcoDynDisconnect()
      } else {
      DBI::dbWriteTable(db_con, DBI::Id(schema = "sfb", table = "seq_runs"), new_run, append = T)
      }
  } else {
    DBI::dbWriteTable(db_con, DBI::Id(schema = "sfb", table = "seq_runs"), new_run, append = T)
  }
  
  
 
  # create tax_query string q_1 for more flexible LIKE ANY query in combination with ARRAY
  if(status == "completed"){
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
       i1 AS i1_2,
       index_seq
     FROM
	     sfb.plate_samples
     LEFT JOIN 
       sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.plate_samples.plate_name
     LEFT JOIN
       sfb.seq_indices ON sfb.seq_indices.index_name = sfb.plate_samples.i1
     WHERE
       sfb.pcr_plates.plate_name LIKE ANY (ARRAY[", q_1, "])
    "
  ))
  
  # create 'sample_tags' folder and create *.txt files for each PCR batch with indices for demultiplexing
  if(dir.exists(paste0(folder,"/sample_tags"))){
    message(paste0("The folder 'sample_tags' already exist in run folder '", folder, "'."))
    overwrite <- utils::select.list(c("Yes, overwrite existing folder", 
                                      "No, save under different name"), 
                                    title = "\nShall the existing folder be replaced?", graphics = FALSE)
    if(grepl("No", overwrite)){
      new_dir <<- paste0(folder,"/sample_tags_", Sys.Date())
      message("\nSFB pipeline needs the folder name 'sample_tags', thus you must rename the created folder if you want to deploy the SFB pipeline.\n")
    } 
  } else {
    new_dir <<- paste0(folder,"/sample_tags")
  }
  
  dir.create(new_dir)

  for(batch in unique(seq_samples$plate_name)){
   write.table(rbind(seq_samples[seq_samples$plate_name == batch, c(2,7,7)], c("negControl", "ATCTG", "ATCTG"), c("posControl", "AACAC", "AACAC")), file = paste0(new_dir,"/", batch, ".txt"), quote = F, row.names = F, col.names = F, sep = " ")
  }
  writeLines(paste0("\nCreated 'sample_tags' folder and i1 lists of each PCR batch for SFB demultiplexing pipeline inside run directory '", folder, "'.\n"))
  DBI::dbWriteTable(db_con, DBI::Id(schema = "sfb", table = "seq_samples"), data.frame(run_name, seq_samples[, c(1:6)]), append = T)
  
  }
  
  
  # end database connection
  if(exists("conn_test") == TRUE){
    EcoDynDisconnect()
  }
  
  
  
}