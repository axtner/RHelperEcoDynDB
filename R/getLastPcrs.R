#' Get info on last PCR batches documented in the EcoDyn database
#' 
#' getLastPcrs 
#' 
#' Function to get list of documented PCR batches from the EcoDyn database. 
#' 
#' @param N Number of PCRs listed
#' 
#' @export

getLastPcrs <- function(N = 5){
  # message
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
  
  # query database
  last_pcr <<- DBI::dbGetQuery(db_con, paste0(
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
                                  sfb.pcrs.plate_name DESC, sfb.pcrs.pcr_date DESC
                                  LIMIT ", N
                                 )
    )
  
  # message
  writeLines(paste0("\nThe last ", N, " PCR batches documented in the EcoDyn DB are the following:"))
  print(last_pcr)
  
}