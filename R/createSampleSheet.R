#' Create SampleSheet for Illumina MiSeq runs
#' 
#' createSampleSheet 
#' 
#' Function to create standardized SampleSheets csv-files for Illumina MiSeq runs. The relevant data queried from the database and is written SampleSheet file.
#' 
#' @param out_dir Folder containing the SampleSheets of a project. If not provided the user is requested select one.
#' 
#' @param pcr_batches Mandatory integers that characterizing the PCR batches that were quantified in the respective xls-files. For example "pcr_batches =c(1:5)" will query the database for the samples of the PCR batches "p001", "p002", "p003", "p004" and "p005", "pcr_batches =c(1, 25, 305)" will query for the PCR batches "p001", "p025" and "p305". 
#' 
#' @export

createSampleSheet = function(out_dir = NA,
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
  
  # test for out_dir
  if(is.na(out_dir) == T){
    if(file.exists("T:/data_BioDiv/") == T){
      out_dir = utils::choose.dir(default = "T:/data_BioDiv/", "Select output directory")
    } else {
      out_dir = utils::choose.dir(default = "Computer", "Select output directory")
    }
  }
  out_dir = gsub("\\\\", "/", out_dir)
  
  
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
  
  
  # function to select run date
  dates_funct <- function(){
    writeLines(paste0("\nThe current date is ", Sys.Date(),".\nWhen do you plan to do the sequencing run?" ))
    date_list <- format(seq(Sys.Date(), length.out = 7, by = "1 day"), "%Y-%m-%d")
    run_date <<- utils::select.list(date_list, title = "Chose the date for the 1. PCR step:")
    
  }
  dates_funct()
  
  # run description
  writeLines("\n")
  description <<- readline("Provide short run description:")
  
  # run description
  writeLines("\n")
  read_no <<- readline("Number of cycles:")
  
  
  
  # create tax_query string q_1 for more flexible LIKE ANY query in combination with ARRAY
  for(i in 1 : length(pcr_batches)){
    q_i = paste0("'", paste0("%", pcr_batches[i], "%"), "'")
    if(i == 1){
      q_1 = q_i
    }
    if(i > 1){
      q_1 = paste(q_1, q_i, sep = ", ")
    }
  }
  
  tab_q = DBI::dbGetQuery(
    db_con,
    paste0(
    "Select plate_name, index_seq FROM sfb.pcr_plates LEFT JOIN sfb.seq_indices on sfb.seq_indices.index_name = sfb.pcr_plates.i2 WHERE plate_name LIKE ANY (ARRAY[", q_1, "])"
    )
  )
   tab_q = tab_q[order(tab_q$plate_name),]
  
  
  # functions to create reverse complement
  seq_rev <- function(char) {
    alphabets <- strsplit(char, split = "")[[1]]
    return(rev(alphabets))
  }
  
  seq_compl <- function(seq) {
    # Check if there's "T" in the sequence
    RNA <- Reduce(`|`, seq == "U")
    cmplvec <- sapply(seq, function(base) {
      # This makes DNA the default
      # As long as there's no U, the sequence is treated as DNA
      if (RNA) {
        switch(base, "A" = "U", "C" = "G", "G" = "C", "U" = "A")
      } else {
        switch(base, "A" = "T", "C" = "G", "G" = "C", "T" = "A")
      }
    })
    return(paste(cmplvec, collapse = ""))
  }
  
  revcom <- function(input) {
    # Make sure the input is character and in upper case
    input <- as.character(input)
    input <- toupper(input)
    
    # Use regular expression to check if there's only legal bases
    # present in the sequence
    legal_char <- Reduce(`&`, grepl("^[A,T,C,G,U]*$", input))
    
    if (!legal_char) {
      stop("revcom() only applies to DNA/RNA sequences, and only A/T/C/G/U is allowed")
    }
    
    rev <- seq_rev(input)
    return(seq_compl(rev))
  }
  
  # create reverse complement to each i2 sequence
  tab_q$index2 = lapply(tab_q$index_seq, revcom)
  
  # test for unique indices
  duplicated_indices <- tab_q$index_seq[duplicated(tab_q$index_seq) | duplicated(tab_q$index_seq, fromLast = TRUE)]
  if (length(duplicated_indices) > 0) {
      dupl_tab <- (tab_q[tab_q$index_seq %in% duplicated_indices, ])
      message("The following PCR batches have identical i2 indices:")
      print(dupl_tab[order(dupl_tab$index_seq),])
      stop("You cannot use identical indices in a sequencing run!")
    } 
  
  
  
  
  
  filename = paste0("SampleSheet_", run_name,".csv")
  setwd(out_dir)
  sink(filename)
  writeLines("[Header]")
  writeLines("Local Run Manager Analysis Id,7007")
  writeLines(paste0("Experiment Name,", run_name))
  writeLines(paste0("Date,", run_date))
  writeLines("Module,GenerateFASTQ - 3.1.0")
  writeLines("Workflow,GenerateFASTQ")
  writeLines("Library Prep Kit,Custom")
  writeLines("Index Kit,Custom")
  writeLines(paste0("Description,", description))
  writeLines("\n")
  writeLines("[Reads]")
  writeLines(read_no)
  writeLines(read_no)
  writeLines("\n")
  writeLines("[Settings]")
  writeLines("\n")
  writeLines("[Data]")
  writeLines("Sample_ID,Sample_Name,Description,I7_Index_ID,index,I5_Index_ID,index2")
  for(i in 1:nrow(tab_q)){
    writeLines(paste(tab_q[i,1], tab_q[i,1], tab_q[i,1], tab_q[i,3], tab_q[i,3], tab_q[i,2], tab_q[i,2],"", sep=","))
  }
  #writeLines("\n")
  sink()
  writeLines(paste0("\nCreated SampleSheet_", run_name,".csv"))
}
  