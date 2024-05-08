#' Update sfb.number_of_pcrs table
#' 
#' updateEcoDyn 
#' 
#' Function thta updates the sfb.number_of_pcrs table of EcoDynDB.
#' 
#' @export

updateEcoDyn= function(){
  # check for database connection and connect if needed
  if(isEcoDynConnected() == FALSE){
    conn_test = FALSE
    writeLines("Please log into the EcoDyn database with your credentials")
    EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # function that updates sfb.number_of_pcrs table 
  DBI::dbExecute(db_con,
                   "WITH temp_latest_data AS (
                    SELECT 
                      extr_name,
                      COUNT(*) AS number_of_pcrs,
                      ARRAY_AGG(CONCAT(pcr_plates.i2, sfb.plate_samples.i1)) AS i2_i1_combinations
                    FROM 
                      sfb.plate_samples
                    LEFT JOIN
                      sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.plate_samples.plate_name
                    LEFT JOIN
                      sfb.pcrs ON sfb.pcrs.plate_name = sfb.plate_samples.plate_name
                    WHERE
                      sfb.pcrs.pcr_no like '1st' and sfb.pcrs.human_blocker like 'NO'
                    GROUP BY
                      extr_name
                    )
                    INSERT INTO sfb.number_of_pcrs (
                      extr_name, 
                      number_of_pcrs, 
                      i2_i1_combinations
                      )
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
  
  DBI::dbExecute(db_con,
                   "WITH temp_latest_data AS (
                    SELECT 
                      extr_name,
                      COUNT(*) AS number_of_pcrs_hb,
                      ARRAY_AGG(CONCAT(pcr_plates.i2, sfb.plate_samples.i1)) AS i2_i1_combinations_hb
                    FROM
                      sfb.plate_samples
                    LEFT JOIN
                      sfb.pcr_plates ON sfb.pcr_plates.plate_name = sfb.plate_samples.plate_name
                    LEFT JOIN
                      sfb.pcrs ON sfb.pcrs.plate_name = sfb.plate_samples.plate_name
                    WHERE
                      sfb.pcrs.pcr_no like '1st' and sfb.pcrs.human_blocker like 'YES'
                    GROUP BY
                      extr_name
                    )
                    INSERT INTO sfb.number_of_pcrs (
                      extr_name, 
                      number_of_pcrs_hb, 
                      i2_i1_combinations_hb
                    )
                    SELECT
                      extr_name,
                      number_of_pcrs_hb,
                      i2_i1_combinations_hb
                    FROM
                      temp_latest_data
                    ON CONFLICT (extr_name) DO UPDATE
                    SET
                      number_of_pcrs_hb = EXCLUDED.number_of_pcrs_hb,
                      i2_i1_combinations_hb = EXCLUDED.i2_i1_combinations_hb"
    )
    writeLines("\nNumber of PCRs per sample in the EcoDynDB were updated")
    
    EcoDynDisconnect()
  }