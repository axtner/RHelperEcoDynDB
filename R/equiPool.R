#' Paper sheets for the equimolar pooling 
#' 
#' equiPool 
#' 
#' Create paper sheets for the equimolar pooling of sequencing libraries from PCR products.
#' 
#' @param out_dir Output directory where the created files are saved.
#' 
#' @param pcr_batches Mandatory integers that characterizing the PCR batches that were quantified in the respective xls-files. For example "pcr_batches =c(1:5)" will query the database for the samples of the PCR batches "p001", "p002", "p003", "p004" and "p005", "pcr_batches =c(1, 25, 305)" will query for the PCR batches "p001", "p025" and "p305". 
#' 
#' @param molarity Desired final molarity of the sequencing library, can be either 4 or 2 nM (default 'molarity = 4'). Always quantify library after pooling!
#' 
#' @export

equiPool = function(out_dir = NA,
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
  
  writeLines("\nequiPool will create files for the batches :")
  writeLines(paste(pcr_batches, collapse=", "))
  writeLines("These files will help you to prepare a equimolar sequencing pool of 2nM.")
  
  
  
  
  doc_file = function(){
    setwd(out_dir)
    filename <- paste0("equiPool_", batch, "_", format(Sys.Date(), "%Y%m%d"), ".txt" )
    sink(filename)
    writeLines("[Header]")
    writeLines("Standardized documentation file for the equimolar pooling of PCR products")
    writeLines(as.character(Sys.Date()))
    writeLines(paste0("Created by R-function equiPool() by DB user ", DBI::dbGetInfo(db_con)$username))
    writeLines("\n")
    writeLines("[LIBRARY INFO]")
    writeLines(paste0("Final molarity:\t\t2nM"))
    writeLines("\n")
    writeLines("[BATCH INFO]")
    writeLines(paste0("PCR batch name:\t\t", batch))
    writeLines(paste0("Batch index i2:\t\t", unique(tab_q$i2)))
    writeLines("Take the listed volume of each PCR product and add the listed volume of water to obtain 10µl of 2nM library per sample. Combine those volume in a single 1.5ml tube to have single library per PCR batch.")
    writeLines("\n")
    writeLines("[BATCH SAMPLES]")
    writeLines("\n")
    writeLines(paste0("line:\textr name:\tdate:\t\ti1:\tsample:\twater:"))
    
    if(nrow(tab_q) >= 17){
      write.table(tab_q[c(1:8),c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE)
      writeLines("--------------------------------------------------------------")
      write.table(tab_q[c(9:16),c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE, row.names = c(9:16))
      writeLines("--------------------------------------------------------------")
      write.table(tab_q[c(17:nrow(tab_q)),c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE, row.names = c(17:nrow(tab_q)))
      writeLines("--------------------------------------------------------------")
    }
    if((nrow(tab_q) < 17) & (nrow(tab_q) > 8)){
      write.table(tab_q[c(1:8),c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE)
      writeLines("--------------------------------------------------------------")
      write.table(tab_q[c(9:nrow(tab_q)),c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE, row.names = c(9:nrow(tab_q)))
      writeLines("--------------------------------------------------------------")
      }
    if(nrow(tab_q) < 9){
      write.table(tab_q[,c(4,2,5,7,8)], append = T, col.names = F, sep = "\t", quote = FALSE)
      writeLines("--------------------------------------------------------------")
      }
    sink()
  }
  
  
    for(batch in pcr_batches){
      tab_q = DBI::dbGetQuery(
        db_con,
        paste0(
          "SELECT
           row_number() OVER (ORDER BY i1) AS row_num,
           date_measure,
           plate_name,
           extr_name,
           i1,
           i2,
           volume_sample,
           volume_water
           FROM (
            SELECT
            sfb.molarities.date_measure,
            sfb.plate_samples.plate_name,
            sfb.plate_samples.extr_name,
            sfb.plate_samples.i1,
            sfb.pcr_plates.i2,
            sfb.molarities.take_µl_sample AS volume_sample,
            sfb.molarities.add_µl_water AS volume_water,
            ROW_NUMBER() OVER (PARTITION BY sfb.plate_samples.extr_name ORDER BY sfb.molarities.date_measure DESC) AS rn
            FROM
            sfb.molarities
            LEFT JOIN sfb.plate_samples ON sfb.plate_samples.extr_name = sfb.molarities.extr_name
            LEFT JOIN sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.molarities.plate_name
            WHERE
            sfb.plate_samples.plate_name = '", batch,"'
            AND
            sfb.pcr_plates.plate_name = '", batch,"'
            ) AS subquery
          WHERE
          rn = 1"
        )
      )
      tab_q$volume_sample = paste0(tab_q$volume_sample, "µl") 
      tab_q$volume_water = paste0(tab_q$volume_water, "µl")
      tab_q$volume_sample = gsub("NAµl", "",  tab_q$volume_sample)
      tab_q$volume_water = gsub("NAµl", "",  tab_q$volume_water)
      doc_file()
    }
  
  writeLines(paste0("\n", Sys.time(), "\nDone!\nHave a nice day!"))
}
    
  
  
  
