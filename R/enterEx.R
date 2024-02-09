#' Enter extraction
#' 
#' enterEx 
#' 
#' Function to enter information on DNA extractions of samples from the EcoDynDB. The extr_names are created and data is written into the EcoDyn database.
#' 
#' @export

enterEx= function(){

  writeLines("\nWelcome to enterEx!\n")    
  
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    writeLines("Please log into the EcoDyn database with your credentials")
    EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <<- get("db_con", envir = .GlobalEnv)
  }
  
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
  
  # Query the projects from the DB
  projects_db <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info")
  
  # Query the projects people from the DB
  people_db <- DBI::dbGetQuery(db_con, "SELECT first_name, family_name, people.people.people_id, proj_id
                                        FROM people.people
                                        LEFT JOIN projects.proj_people on people.people.people_id = projects.proj_people.people_id")
  
  people_db$fullname <- paste(people_db$first_name, people_db$family_name)
  people_db$fullname <- gsub("unknown unknown", "unknown", people_db$fullname)
  
  # Query the sample_names and their sample_id
  samples_db <- DBI::dbGetQuery(db_con, "SELECT * FROM nuc_acids.samples")
  
  filter_funct <- function(){
    # Chose project
    proj_name <<- utils::select.list(projects_db[order(projects_db$proj_year, decreasing =T),]$proj_name, 
                                  title = "\nSelect project:", graphics = FALSE)
    project <<- projects_db[projects_db$proj_name == proj_name,]
      
    # Chose sample type
    type <<- utils::select.list(unique(samples_db$sample_type), 
                               title = "\nSelect sample type:", graphics = FALSE)
    
    preselect <<- samples_db[samples_db$proj_id == project$proj_id & samples_db$sample_type == type,]
  
    # sample name
    name_funct <<- function(){
      name_filter <<- readline("Type part of sample name: ")
      
      sample_name <<- utils::select.list(preselect$sample_name[grepl(name_filter, preselect$sample_name, ignore.case = T)], 
                                         title = "\nChose sample or chose '0' to re-define filter:")
      sample_id <<- samples_db$sample_id[samples_db$sample_name == sample_name]
    }
    
    name_funct()
    
    while(sample_name == ""){
      name_funct()
    }
    
    performer <<- utils::select.list(people_db$fullname[grepl(project$proj_id, people_db$proj_id, ignore.case = T)], 
                                     title = "\nChose sample or chose '0' to re-define filter:")
    people_id <- people_db$people_id[people_db$fullname == performer & people_db$proj_id == project$proj_id]
    
    writeLines("\nType date (e.g.2024-01-31) of final processing step. ")
    date_extr <- readline("Date: ")
    date_extr <<- as.Date(date_extr)
    
    if(type == "leech"){
      n_leeches <- readline("\nNumber of leeches: ")
      leechs_type <- utils::select.list(c("unknown", "brown", "tiger"), title = "\nLeech type:")
      new_leeches <<- data.frame(sample_id, leechs_type, n_leeches)
    }
    
    type_extr_funct<- function(){
      type_extr <<- utils::select.list(c("DNA", "RNA", "DNA & RNA"), 
                                       title = "\nChose typ of extraction:")
    }
    type_extr_funct()
    
    while(type_extr == ""){
      message("\nCannot stay empty!")
      type_extr_funct()
    }
    
    writeLines("\nType any comment? Do not use line breaks")
    comm_extr <- readline("Comments:")
    if(comm_extr == ""){comm_extr = NA}
    
    A <<- data.frame(extr_name = paste0(sample_name, "_a"), sample_id, sample_name, date_extr, type_extr, people_id, comm_extr, created_by = DBI::dbGetInfo(db_con)$username)
    B <<- data.frame(extr_name = paste0(sample_name, "_b"), sample_id, sample_name, date_extr, type_extr, people_id, comm_extr, created_by = DBI::dbGetInfo(db_con)$username)
    new_extr <<- rbind(A,B)
  }
  
  conf_funct <- function(){
    message("\nPlease check the following carefully before you proceed!")
    writeLines(paste0("\nName of the sample is:\t\t\t", sample_name))
    writeLines(paste0("\nExtracted by:\t\t\t", performer))
    writeLines(paste0("\nFinal date of extraction:\t\t\t", date_extr))
    writeLines(paste0("\nNucleic acid extracted:\t\t\t", type_extr))
    conf1 <<- utils::select.list(c("Yes, I everything is correct. Save data to database.", 
                                   "No, there is something wrong. Please, let me correct it.", 
                                   "Exit without save."), 
                                title = "\nPlease confirm that eveything is correct:")
   if(grepl("Exit", conf1)){stop_quietly()}
   if(grepl("No", conf1)){filter_funct()}
   if(grepl("Yes", conf1)){ 
     t1 <- DBI::dbAppendTable(db_con, DBI::Id(schema = "nuc_acids", table = "extractions"), new_extr)
     
     if(type == "leech"){
       t2 <- DBI::dbAppendTable(db_con, DBI::Id(schema = "nuc_acids", table = "leeches"), new_leeches)
       }
     }
   }
  
  cont_funct <- function(){
    conf2 <<- utils::select.list(c("Yes, I am eager to work.", 
                                   "No, let me out of this hell."), 
                                 title = "\nDo you want to continue?")
    if(grepl("Yes", conf2) == TRUE){
      while(grepl("Yes", conf2) == TRUE){
        writeLines("\nYou are a diligent worker.Very Nice!\n")
        filter_funct()
        conf_funct()
        conf2 <<- utils::select.list(c("Yes, I am eager to work.", 
                                       "No, let me out of this hell."), 
                                     title = "\nDo you want to continue?")
      }
    } else {
      message("\nSad to see you go. Have nice day!\n")
      stop_quietly()
    }
  }
  
  filter_funct()
  conf_funct()
  cont_funct()
  
}
