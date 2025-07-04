#' Get samples for new PCR
#' 
#' createPcr 
#' 
#' Function to get list of unprocessed samples from the EcoDyn database and to calculate reagents of PCR. The relevant data is written into the EcoDyn database and some tables are updated.
#' 
#' @param out_dir Folder where the documentation files are saved.
#' 
#' @export

createPcr= function(out_dir = NA){
    

writeLines("\nWelcome!\nYou decided to do some lab work. Great!\nThis function will help you to query the correct samples from the EcoDyn database.\nIt will calculate the PCR mastermixes for you and will write you a standardized PCR info file, which you can print out and take to the lab.\nThe function will also write the data into the EcoDyn database and update relevant tables therein.\n")    
  
# check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    writeLines("Please log into the EcoDyn database with your credentials")
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }

  
# exit function  
  stop_quietly <<- function() {
    opt <- options(show.error.messages = F)
    on.exit(options(opt))
    if(exists("conn_test") == TRUE){
      EcoDynDisconnect()
    }
    message("\nYou decided to stop the process. Have a nice day!")
    
    stop()
  }
  
  
# function to set output repository for PCR documentation file
  dir_funct<- function(){
    #if(exists("out_dir", envir = .GlobalEnv) == T){
    #out_dir <<- get("out_dir", envir = .GlobalEnv)
    #} else {
    #  out_dir = NA
    #  }
    if(is.na(out_dir) == T){
      if(file.exists("T:/data_BioDiv/") == T){
        out_dir <<- utils::choose.dir(default = "T:/data_BioDiv/", "Select folder to save the PCR documentation files")
      } else {
        out_dir <<- utils::choose.dir(default = "Computer", "Select folder to save the PCR documentation files")
      }
    }
  }
  dir_funct()
  
  
# Query the projects from the DB and chose project
  projects_db <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info")
  
# Query the last pcrs , their dates and their indices i2
  lpcr_funct <- function(){
    last_pcr <<- DBI::dbGetQuery(db_con, 
                              "SELECT 
                                sfb.pcrs.plate_name,
                                sfb.pcrs.pcr_date,
                                sfb.pcr_plates.i2
                               FROM 
                                sfb.pcrs
                               LEFT JOIN
                                sfb.pcr_plates ON sfb.pcrs.plate_name = sfb.pcr_plates.plate_name
                               WHERE 
                                sfb.pcrs.pcr_no LIKE '2nd'
                               ORDER BY 
                                cast(replace(sfb.pcrs.plate_name, 'p', '') as integer) desc, sfb.pcrs.pcr_date DESC
                               LIMIT 5"
  )
  #writeLines("\nThe last five PCRs documented in the EcoDyn DB had the following i2 indices:")
  #print(last_pcr)
  }
  lpcr_funct()
  
  
  
# Query the sample_names and their sample_id
  samples_db <- DBI::dbGetQuery(db_con, "SELECT * FROM nuc_acids.samples")
  
  
# preselect project and sample type    
  presel_funct <- function(){
    # Chose project
    proj_name <<- utils::select.list(projects_db[order(projects_db$proj_year, decreasing =T),]$proj_name, 
                                   title = "\nSelect project:", graphics = FALSE)
    project <<- projects_db[projects_db$proj_name == proj_name,]
  
    # Chose sample type
    type <<- utils::select.list(unique(samples_db$sample_type), 
                                title = "\nSelect sample type:", graphics = FALSE)
  }
  presel_funct()
  
  
# function to select batch name
  name_funct <- function(){
    name_opt <- paste0("p", as.numeric(gsub("p","", last_pcr$plate_name[1]))+1)
    writeLines("\nThe last five PCRs documented in the EcoDyn DB had the following i2 indices:")
    print(last_pcr)
    writeLines(paste0("\nThe name of the last PCR batch was '", last_pcr$plate_name[1], "'.\nBased on that we suggest '", name_opt, "' as name for the new batch."))
    fb1 <<- utils::select.list(c(paste0("Yes, '", name_opt, " is correct."), "No, let me enter a different name."), title = "\nIs that correct?", graphics = FALSE)
    if(grepl("No|Yes", fb1) == F){fb1 <<- "No"}
    if(grepl("Yes", fb1) == T){bname <<- name_opt}
    if(grepl("No", fb1) == T){
      while(grepl("No", fb1) == T){
        bname <<- readline("\nPlease enter a the name of the new PCR batch:")
        fb1 <<- utils::select.list(c(paste0("Yes, '", bname, "' is correct."), "No, let me enter a different name."), title = paste0("\nYou entered '", bname, "' as batch name.\nIs that correct?"), graphics = FALSE)
        if(grepl("No|Yes", fb1) == F){fb1 <<- "No"}
        if(bname == ""){
          message(paste0("You must enter a new batch name or enter the suggested name '", name_opt, "'."))
          fb1 <<- "No"
        }
      }
    } 
    
  }
  name_funct()
  
  
# function to select genetic marker
  marker_funct <- function(){
    marker <<- utils::select.list(c("16S", "12S", "CytB"), title = "\nPlease select the genetic marker:", graphics = FALSE)
  }
  marker_funct()


  # function to select for use of human blocker
  hb_funct <- function(){
    hblocker <<- utils::select.list(c("NO", "YES"), title = "\nUse human blocker:", graphics = FALSE)
  }
  hb_funct()
  
  
# function to select i2 index
  i2_funct <- function(){
    i2_list <<- c(LETTERS[c(1:9, 11:20, 22:26)])
    index <- which(i2_list == last_pcr$i2[1])
    if(index + 1 == 25){new_index <<- 1} else {new_index <<- index + 1}
    writeLines(paste0("\nThe last used i2 index was '", last_pcr$i2[1], "', thus the suggested index for the present PCR batch is now '", i2_list[new_index], "'."))
    fb2 <<- utils::select.list(c(paste0("Yes, '", i2_list[new_index], "' is correct."), "No, let me chose another i2 index."), title = "Is that correct?", graphics = FALSE)
    if(grepl("No|Yes", fb2) == F){fb2 <<- "No"}    
    if(grepl("Yes", fb2) == T){i2 <<- i2_list[new_index]}
    if(grepl("No", fb2) == T){
      while(grepl("No", fb2) == T){
        i2 <<- utils::select.list(i2_list, title = "Please select an i2 index:", graphics = FALSE)
        fb2 <<- utils::select.list(c(paste0("Yes, '", i2, "' is correct."), "No, let me enter a different index."), title = paste0("\nYou entered '", i2, "' as index.\nIs that correct?"), graphics = FALSE)
        if(i2 == "" | i2 == "NA"){
          message(paste0("You must enter an i2 index."))
          fb2 <<- "No"
        }
      }
    }
  }
  i2_funct()
  

# function to select reaction and template volumes
  vol1 <<- "15 µl"
  vol2 <<- "20 µl"
  vol3 <<- "2 µl"
  vol4 <<- "5 µl"
    
  
# functon to define reaction volume
  volumes_funct <- function(){
    writeLines(paste0("\nThe reaction volume for the 1. PCR step is ", vol1, "."))
    writeLines(paste0("\nThe reaction volume for the 2. PCR step is ", vol2, "."))
    writeLines(paste0("\nThe template volume for the 1. PCR step is ", vol3, "."))
    writeLines(paste0("\nThe template volume for the 2. PCR step is ", vol4, "."))
    
    fb3 <<- utils::select.list(c("No", "Yes"), title = paste0("\nDo you want to change any of those volumes?"))
    while(grepl("Yes", fb3) == T){
      fb4 <<- utils::select.list(c("1. PCR reaction volume", 
                                   "2. PCR reaction volume",
                                   "1. PCR template volume",
                                   "2. PCR template volume"), multiple = TRUE,
                                 title = "\nSelect volumes you want to change", graphics = FALSE)
      if("1. PCR reaction volume" %in% fb4){
        vol1 <<- utils::select.list(paste(seq(10, 30, 5), "µl"), title = "\nPlease adjust the recation volume of the 1. PCR step.", graphics = FALSE)
      }
      if("2. PCR reaction volume" %in% fb4){
        vol2 <<- utils::select.list(paste(seq(10, 30, 5), "µl"), title = "\nPlease adjust the recation volume of the 2. PCR step.", graphics = FALSE)
      }
      if("1. PCR template volume" %in% fb4){
        vol3 <<- utils::select.list(paste(seq(1, 5, 1), "µl"), title = "\nPlease adjust the template volume of the 1. PCR step.", graphics = FALSE)
      }
      if("2. PCR template volume" %in% fb4){
        vol4 <<- utils::select.list(paste(seq(1, 5, 1), "µl"), title = "\nPlease adjust the template volume of the 2. PCR step.", graphics = FALSE)
      }
      writeLines(paste0("\nThe volumes are now:"))
      writeLines(paste0("\nThe reaction volume for the 1. PCR step is ", vol1, "."))
      writeLines(paste0("\nThe reaction volume for the 2. PCR step is ", vol2, "."))
      writeLines(paste0("\nThe template volume for the 1. PCR step is ", vol3, "."))
      writeLines(paste0("\nThe template volume for the 2. PCR step is ", vol4, "."))
      fb3 <<- utils::select.list(c(paste0("No, volumes are fine."), "Yes"),
                                 title = paste0("\nYou want to change them?", graphics = FALSE))
      
    }
  }
  volumes_funct()
   
  
# function to select PCR dates
  dates_funct <- function(){
    writeLines(paste0("\nThe current date is ", Sys.Date(),".\nWhen will you run the 1. and the 2. PCR for this batch?" ))
    date_list <- format(seq(Sys.Date(), length.out = 7, by = "1 day"), "%Y-%m-%d")
    date1 <<- utils::select.list(date_list, title = "Chose the date for the 1. PCR step:")
    date2 <<- utils::select.list(date_list, title = "Chose the date for the 2. PCR step:")
  }
  dates_funct()


# function to query samples from database
  # list of index i1  
  i1 <<- c(LETTERS[c(1:9, 11:20, 22:26)], "ctr")
  # function
  samples_funct <- function(){
    if(ext_rep == "A"){
      cond_1 <<- "nuc_acids.extractions.extr_name LIKE '%_a'"
      } else {
        cond_1 <<- "nuc_acids.extractions.extr_name LIKE '%_b'"
        }
    #if(ext_rep == ""){stop()}
    
    if(hblocker == "NO"){
    cond_2 <<- paste0("sfb.number_of_pcrs.number_of_pcrs = ", as.numeric(pcr_rep)-1)
    } else {
      cond_2 <<- paste0("sfb.number_of_pcrs.number_of_pcrs_hb = ", as.numeric(pcr_rep)-1)
    }
    
  # query samples that need to be processed next from the database
    if(hblocker == "NO"){
      if(pcr_rep == 1){
        samples <<- DBI::dbGetQuery(db_con, 
                                    paste0(
                                      "SELECT
                                       nuc_acids.extractions.extr_name, 
                                       nuc_acids.extractions.extract_id
                                       FROM 
                                       nuc_acids.extractions
                                       LEFT JOIN
                                       nuc_acids.samples ON nuc_acids.extractions.sample_name = nuc_acids.samples.sample_name
                                       WHERE 
                                       nuc_acids.samples.proj_id = '", project$proj_id,"'
                                       AND 
                                       nuc_acids.samples.sample_type = '", type, "'
                                       AND ", 
                                       cond_1, "
                                       AND
                                       nuc_acids.extractions.extr_name NOT IN 
                                       (SELECT 
                                        plate_samples.extr_name 
                                        FROM 
                                        sfb.plate_samples
                                        LEFT JOIN
                                        sfb.pcrs ON sfb.pcrs.plate_name = sfb.plate_samples.plate_name
                                        WHERE
                                        sfb.pcrs.pcr_no like '1st' and sfb.pcrs.human_blocker like 'NO')
                                       ORDER BY 
                                       nuc_acids.extractions.extr_name 
                                       LIMIT 24" 
                                      )
                                    )
        samples$number_of_pcrs = rep(0, nrow(samples))
        } else {
        samples <<- DBI::dbGetQuery(db_con, 
                                    paste0(
                                    "SELECT 
                                     nuc_acids.extractions.extr_name, 
                                     nuc_acids.extractions.extract_id,
                                     sfb.number_of_pcrs.number_of_pcrs
                                     FROM 
                                     nuc_acids.extractions
                                     LEFT JOIN
                                     sfb.number_of_pcrs 
                                     ON nuc_acids.extractions.extr_name = number_of_pcrs.extr_name
                                     LEFT JOIN
                                     nuc_acids.samples 
                                     ON nuc_acids.extractions.sample_name = nuc_acids.samples.sample_name
                                     WHERE 
                                     nuc_acids.samples.proj_id = '", project$proj_id,"'
                                     AND 
                                     nuc_acids.samples.sample_type = '", type, "'
                                     AND ", 
                                     cond_1, "
                                     AND ", 
                                     cond_2, " 
                                     ORDER BY 
                                     nuc_acids.extractions.extr_name
                                     LIMIT 24"
                                    )
                                  )
    }
      } else {
        if(pcr_rep == 1){
          samples <<- DBI::dbGetQuery(db_con, 
                                      paste0(
                                        "SELECT
                                       nuc_acids.extractions.extr_name, 
                                       nuc_acids.extractions.extract_id
                                       FROM 
                                       nuc_acids.extractions
                                       LEFT JOIN
                                       nuc_acids.samples ON nuc_acids.extractions.sample_name = nuc_acids.samples.sample_name
                                       WHERE 
                                       nuc_acids.samples.proj_id = '", project$proj_id,"'
                                       AND 
                                       nuc_acids.samples.sample_type = '", type, "'
                                       AND ", 
                                        cond_1, "
                                       AND
                                       nuc_acids.extractions.extr_name NOT IN 
                                       (SELECT 
                                        plate_samples.extr_name 
                                        FROM 
                                        sfb.plate_samples
                                        LEFT JOIN
                                        sfb.pcrs ON sfb.pcrs.plate_name = sfb.plate_samples.plate_name
                                        WHERE
                                        sfb.pcrs.pcr_no like '1st' and sfb.pcrs.human_blocker like 'YES')
                                       ORDER BY 
                                       nuc_acids.extractions.extr_name 
                                       LIMIT 24"
                                      )
          )
          samples$number_of_pcrs = rep(0, nrow(samples))
        } else {  
      samples <<- DBI::dbGetQuery(db_con, 
                                  paste0(
                                    "SELECT 
                                     nuc_acids.extractions.extr_name, 
                                     nuc_acids.extractions.extract_id,
                                     sfb.number_of_pcrs.number_of_pcrs_hb
                                     FROM 
                                     nuc_acids.extractions
                                     LEFT JOIN
                                     sfb.number_of_pcrs 
                                     ON nuc_acids.extractions.extr_name = number_of_pcrs.extr_name
                                     LEFT JOIN
                                     nuc_acids.samples 
                                     ON nuc_acids.extractions.sample_name = nuc_acids.samples.sample_name
                                     WHERE 
                                     nuc_acids.samples.proj_id = '", project$proj_id,"'
                                     AND 
                                     nuc_acids.samples.sample_type = '", type, "'
                                     AND ", 
                                     cond_1, "
                                     AND ", 
                                     cond_2, " 
                                     ORDER BY 
                                     nuc_acids.extractions.extr_name
                                     LIMIT 24" 
                                  )
      )
        }
      }
  
  if(nrow(samples) > 0){
    samples$number_of_pcrs[is.na(samples$number_of_pcrs)] <- 0
    samples <<- data.frame(samples[,c(1,3)], i1 = i1[1:nrow(samples)], plate_name = bname)
    }
  }
  for(ext_rep in c("A", "B")){
    print(ext_rep)
    for(pcr_rep in c(1, 2, 3)){
      print(pcr_rep)
      samples_funct() 
      if(nrow(samples) != 0){
        break
      } 
    }
    if(nrow(samples) != 0){
      break
    }
  }
  

# calculation of PCR mastermixes
  mastermix <- function(){
    N <<- nrow(samples)
    nvol1 <<- as.numeric(gsub(" µl", "", vol1))
    nvol2 <<- as.numeric(gsub(" µl", "", vol2))
    nvol3 <<- as.numeric(gsub(" µl", "", vol3))
    nvol4 <<- as.numeric(gsub(" µl", "", vol4))
  
    if(hblocker == "NO"){
      buffer1 <<- 0.2*nvol1*1.1*(N+2)
      MgCl1 <<- 0.12*nvol1*1.1*(N+2)
      dNTPs1 <<- 0.008*nvol1*1.1*(N+2)
      taq1 <<- 0.005*nvol1*1.1*(N+2)
      primer1 <<- 0.1*nvol1*1.1
      water1 <<- (nvol1-nvol3-primer1)*(N+2)*1.1-(buffer1+MgCl1+dNTPs1+taq1)
    } else {
      buffer1 <<- 0.2*nvol1*1.1*(N+2)
      MgCl1 <<- 0.12*nvol1*1.1*(N+2)
      dNTPs1 <<- 0.008*nvol1*1.1*(N+2)
      taq1 <<- 0.005*nvol1*1.1*(N+2)
      hblock1 <<- 0.05*nvol1*1.1*(N+2)
      primer1 <<- 0.1*nvol1*1.1
      water1 <<- (nvol1-nvol3-primer1)*(N+2)*1.1-(buffer1+MgCl1+dNTPs1+taq1+hblock1)
    }
  
    buffer2 <<- 0.2*nvol2*1.1*(N+2)
    MgCl2 <<- 0.12*nvol2*1.1*(N+2)
    dNTPs2 <<- 0.008*nvol2*1.1*(N+2)
    taq2 <<- 0.005*nvol2*1.1*(N+2)
    primer2 <<- 0.1*nvol2*1.1*(N+2)
    water2 <<- (nvol2-nvol4)*(N+2)*1.1-(buffer2+MgCl2+dNTPs2+taq2+primer2)
  }
  mastermix()

  
# function that writes to database 
  write_db <- function(){
    new_pcrs <- data.frame(pcr_date = c(date1, date2), plate_name = bname, pcr_no = c("1st", "2nd"), rxn_vol = c(nvol1, nvol2), template_vol = c(nvol3, nvol4), gen_marker = marker, created_by = DBI::dbGetInfo(db_con)$username, human_blocker = c(hblocker, hblocker))
    
    t1 <- DBI::dbAppendTable(db_con, DBI::Id(schema="sfb", table="pcr_plates"), data.frame(plate_name = bname, i2))
    
    t2 <- DBI::dbAppendTable(db_con, DBI::Id(schema="sfb", table="plate_samples"), data.frame(extr_name = samples[,1], plate_name = bname, i1 = i1[1:nrow(samples)]))
    
    v1 <- DBI::dbGetQuery(db_con, "SELECT plate_name FROM sfb.pcr_plates")
    
    while(bname %in% v1$plate_name == FALSE){
      v1 <- DBI::dbGetQuery(db_con, "SELECT plate_name FROM sfb.pcr_plates")
    }
    
    t3 <- DBI::dbAppendTable(db_con, DBI::Id(schema = "sfb", table = "pcrs"), new_pcrs)
    writeLines("\nWriting data to EcoDynDB")
    
  }
    
  
# function that writes PCR documentation file
  doc_file <- function(){
    setwd(out_dir)
    filename <- paste0(bname, "_", strsplit(date1, "-")[[1]][3], "-", strsplit(date1, "-")[[1]][2], "_", strsplit(date2, "-")[[1]][3], "-", strsplit(date2, "-")[[1]][2], "-", strsplit(date2, "-")[[1]][1], ".txt" )
    sink(filename)
    writeLines("[Header]")
    writeLines("Standardized PCR documentation file")
    writeLines(as.character(Sys.Date()))
    writeLines(paste0("Created by R-function createPcr() by DB user ", DBI::dbGetInfo(db_con)$username))
    writeLines("\n")
    writeLines("[BATCH INFO]")
    writeLines(paste0("PCR batch name:\t\t", bname))
    writeLines(paste0("Batch index i2:\t\t", i2))
    writeLines(paste0("1st PCR date:\t\t", date1))
    writeLines(paste0("2nd PCR date:\t\t", date2))
    writeLines(paste0("Genetic marker:\t\t", marker))
    writeLines(paste0("human blocker used:\t\t", hblocker))
    writeLines("\n")
    writeLines("[BATCH SAMPLES]")
    if(nrow(samples) < 9){ 
      write.table(samples[,1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE) 
    }
    if((nrow(samples) >= 8) & (nrow(samples) < 16)){ #! changed from 9 to 8, and from 17 to 16
      write.table(samples[c(1:8),1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE) 
      writeLines("---------------------------------------")
      write.table(samples[c(9:nrow(samples)),1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE, row.names = c(9:nrow(samples))) 
    }
    if(nrow(samples) >= 17){  
      write.table(samples[c(1:8),1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE)
      writeLines("---------------------------------------")
      write.table(samples[c(9:16),1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE, row.names = c(9:16))  
      writeLines("---------------------------------------")
      write.table(samples[c(17:nrow(samples)),1], append = T, col.names = F, sep = "\t\t\t", quote = FALSE, row.names = c(17:nrow(samples)))  
    }
    writeLines("\n")
    writeLines("[1. PCR MASTERMIX]")
    writeLines(paste0("1st PCR reaction volume:\t", vol1))
    writeLines(paste0("Water:\t\t\t", round(water1, digits = 2), " µl" ))
    writeLines(paste0("Buffer[5x]:\t\t", round(buffer1, digits = 2), " µl"))
    writeLines(paste0("MgCl[25mM]:\t\t", round(MgCl1, digits = 2), " µl"))
    writeLines(paste0("dNTPs[25mM each]:\t", round(dNTPs1, digits = 2), " µl"))
    if(hblocker == "YES"){
      writeLines(paste0("Human blocker [10µM]:\t", round(hblock1, digits = 2), " µl"))
    }
    writeLines(paste0("Taq[5u/µl]:\t\t", round(taq1, digits = 2), " µl"))
    writeLines(paste0("\nDistribute ", round(nvol1 - (1.1 + nvol3), digits = 2), " µl to each well"))
    writeLines("\nAdd to each well:")
    writeLines(paste0("Primer[1µM]:\t\t", round(primer1, digits = 2), " µl"))
    writeLines(paste0("DNA template:\t\t", round(nvol3, digits = 2), " µl"))
    writeLines("\n")
    writeLines("[2. PCR MASTERMIX]")
    writeLines(paste0("2nd PCR reaction volume:\t", vol2))
    writeLines(paste0("Water:\t\t\t", round(water2, digits = 2), " µl" ))
    writeLines(paste0("Buffer[5x]:\t\t", round(buffer2, digits = 2), " µl"))
    writeLines(paste0("MgCl[25mM]:\t\t", round(MgCl2, digits = 2), " µl"))
    writeLines(paste0("dNTPs[25mM each]:\t", round(dNTPs2, digits = 2), " µl"))
    writeLines(paste0("Taq[5u/µl]:\t\t", round(taq2, digits = 2), " µl"))
    writeLines(paste0("Primer[1µM]:\t\t", round(primer2, digits = 2), " µl"))
    writeLines(paste0("\nDistribute ", round(nvol2 - nvol4, digits = 2), " µl to each well"))
    writeLines("\nAdd to each well:")
    writeLines(paste0("DNA template:\t\t", round(nvol4, digits = 2), " µl"))
    sink()
    writeLines(paste0("\nPCR documentation file '", filename , "' was written to '", out_dir, "'."))
    
    # delete objects from previous entries
    remove(list=c((objects()[grepl("db_|out_|stop", objects()) == F])))
  }

# function to get confirmation by user
  conf_funct <- function(){
    # summary
    if(nrow(samples) == 0){
      message("\nNo samples found to be processed!\n")
    } else {
      writeLines(paste0("\nBased on extraction replicate '", ext_rep, "' and the PCR replicate '", pcr_rep, "' the next ", N, " samples that should be processed are:"))
    print(samples[,c(1,2)])
    writeLines("Please note that 'number_of_pcrs' reflects the current database status.")
    }
  
    message("\nPlease check the following carefully before you proceed!")
    writeLines(paste0("\nName of the project is:\t\t\t\t", proj_name))
    writeLines(paste0("\nType of the samples is:\t\t\t\t", type))
    writeLines(paste0("\nName of the PCR batch is:\t\t\t", bname))
    writeLines(paste0("\nIndex of previous PCR batch was '", last_pcr$i2[1], "',\nthe index of current PCR batch is:\t\t", i2))
    writeLines(paste0("Dates of 1. and 2. PCR steps are:\t\t", date1, ", ", date2))
    writeLines(paste0("\nThe genetik marker is:\t\t\t\t", marker))
    writeLines(paste0("\nHuman blocker used:\t\t\t\t", hblocker))
    writeLines(paste0("\nReaction volumes of 1. and 2. PCR step are:\t", vol1, ", ", vol2))
    writeLines(paste0("\nTemplate volumes of 1. and 2. PCR step are:\t", vol3, ", ", vol4))
    writeLines(paste0("\nThe PCR documentation file is saved:\t\t", out_dir))
  
    # warning 
    message("\nCan you confirm that those setting are correct?")
    writeLines("\nIf you will answer the following question with 'Yes' the  data is written to the EcoDyn database and you won't be able to undo this. In case you are unsure, please contact the database admin.")
    
    conf <<- utils::select.list(c("Yes, I everything is correct. Save data to database.", 
                                  "No, there is something wrong. Please, let me correct it.", 
                                  "Exit without save."), 
                                title = "\nPlease confirm that eveything is correct:")
  }

  
# function for check
  change_funct <- function(){
    change_what <<- utils::select.list(c("project or sample type",
                                         "PCR batch name",
                                         "batch index",
                                         "PCR dates",
                                         "genetic marker",
                                         "human blocker",
                                         "reaction or template volumes",
                                         "output directory"),
                                       title = "\nPlease select the parameters you want to change:",
                                       multiple = TRUE, graphics = FALSE
    )
    if("project or sample type" %in% change_what){
      presel_funct()
    }
    if("PCR batch name" %in% change_what){
      name_funct()
    }
    if("batch index" %in% change_what){
      i2_funct()
    }
    if("PCR date" %in% change_what){
      dates_funct()
    }
    if("genetic marker" %in% change_what){
      marker_funct()
    }
    if("human blockerr" %in% change_what){
      hb_funct()
    }
    if("reaction or template volumes" %in% change_what){
      volumes_funct()
    }
    if("output directory" %in% change_what){
      dir_funct()
    }
    # ask for confirmation again
    conf_funct()
  }

  
# function for repetition
  continue_funct <- function(){
    continue <<-  utils::select.list(c("No, please let me go", "Yes, let me continue"),
                                     title = "\nDo you want to continue with the next PCR batch?", graphics = FALSE)
    if(grepl("Yes", continue) == TRUE){
      while(grepl("Yes", continue) == TRUE){
        writeLines("\nYou are a diligent lab worker as you decided continue with the next PCR batch.\nVery Nice!\n")
        presel_funct()
        lpcr_funct()
        name_funct()
        marker_funct()
        hb_funct()
        i2_funct()
        volumes_funct()
        dates_funct()
        samples_funct()
        mastermix()
        conf_funct()
        while(grepl("No", conf) == T){
          change_funct()
        }
        if(grepl("Yes", conf) == T){
          write_db()
          doc_file()
          updateEcoDyn()
        }
        if(grepl("Exit", conf) == T){
          stop_quietly()
        }
        
        continue <<-  utils::select.list(c("No, please let me go", "Yes, let me continue"),
                                        title = "\nDo you want to continue with the next PCR batch?", graphics = FALSE)
      }
    } else {
      message("\nSad to see you go. Have nice day!\n")
      stop_quietly()
    }
  }
  
  
# query confirmation
  conf_funct()
  
# exit if option 3
  if(grepl("Exit", conf) == T){stop_quietly()}

# restart if option 2
  while(grepl("No", conf) == T){
    change_funct()
  }
  
  
  
# save to database if option 1 
  if(grepl("Yes", conf) == T){
    write_db()
    doc_file()
    # message
    writeLines(paste0("\nWrote data of ", bname, " (i2: ", i2, "), to the EcoDyn database.\n"))
    updateEcoDyn()
    continue_funct()
  }
}
