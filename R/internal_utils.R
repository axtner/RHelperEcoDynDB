#' internal functions
#' 
#' internal helper functions
#' 
#' @keywords internal

# function for platform dependent folder selection ----
.selectDir <- function() {
  
  # 1) test for Rstudio-API 
  in_rstudio <- FALSE
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    in_rstudio <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
  }
  
  if (in_rstudio) {
    return(rstudioapi::selectDirectory(caption = "Select Directory"))
  }
  
  
 # sys <- Sys.info()["sysname"]
  
  # 2) Windows (PowerShell folder dialogue)
  if (.Platform$OS.type == "windows") {
    res <- shell(
      'powershell -command "Add-Type -AssemblyName System.Windows.Forms; 
       $f = New-Object System.Windows.Forms.FolderBrowserDialog; 
       $null = $f.ShowDialog(); 
       $f.SelectedPath"',
      intern = TRUE
    )
    if (length(res) > 0 && nzchar(res)) return(res)
  }
  
  # 3) macOS (Finder dialogue)
  if (Sys.info()["sysname"] == "Darwin") {
    res <- system(
      'osascript -e \'tell app "Finder" to POSIX path of (choose folder)\'',
      intern = TRUE
    )
    if (length(res) > 0 && nzchar(res)) return(res)
  }
  
  # 4) Linux (zenity, if installed)
  if (Sys.info()["sysname"] == "Linux") {
    res <- system("zenity --file-selection --directory", intern = TRUE)
    if (length(res) > 0 && nzchar(res)) return(res)
  }
  
  # 5) Fall back: manual entry
  message("Please enter folder path:")
  pfad <- readline("> ")
  if (nzchar(pfad)) return(pfad)
  
  return(NA)
}



# Function to format and run SQL scripts from R. Use DBI::dbGetQuery(db_con, getSQL("CHANGEthisFILE.sql")) ----
.readSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""
  
  while (TRUE){
    line <- readLines(con, n = 1)
    
    if ( length(line) == 0 ){
      break
    }
    
    line <- gsub("\\t", " ", line)
    
    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}