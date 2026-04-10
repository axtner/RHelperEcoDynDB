#' internal functions
#' 
#' internal helper functions
#' 
#' @keywords internal

# function for platform dependent folder selection. ----
.selectDir <- function(){
  
  # 1) test for Rstudio-API 
  in_rstudio <- FALSE
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    in_rstudio <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
  }
  
  if (in_rstudio) {
    return(rstudioapi::selectDirectory(caption = "Select Directory"))
  }
  
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



# Function to format and run SQL scripts from R. ----
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


# Function to select one or more EcoDynDB projects. ----
.selectProject = function(){
  message("You have to select a project from the EcoDynDB? You can filter the existing projects by one ore more filter options.")
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # read proj_info table from database
  projects <- DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "proj_info"))
  
  
  # select filter
  filter <- function(){
    selFilter <- utils::select.list(c("Project name", "Start year or range of years", "Keywords", "Paricipants", "Organisations", "No filter", "Exit"), title = "Please choose from the following filter options:", multiple = T, graphics = FALSE)
    
    if("Exit" %in% selFilter){
      stop("\rYou chose 'Exit' and decided to leave. Have a nice day!")
    }
    
    if("No filter" %in% selFilter){
      message("\nYou decided to use 'No filter', which overrides all selected filters.")
      selFilter <- "No filter"
    }
    
    # filter 1, project name ----
    if("Project name" %in% selFilter){
      message("Filter by project name or parts of project name.")
      f1 = readline("Enter partial project name:")
      f1_projects = projects[grepl(tolower(f1), tolower(projects$proj_name)),]
      f1_projects = f1_projects[order(f1_projects$proj_name),]
      rm(f1)
    }# end of filter 1
    
    
    # filter 2, start year ----
    if("Start year or range of years" %in% selFilter){
      message("Filter by start year of project.")
      f2 = readline("Enter year or range of years (e.g. 2023:2026):")
      if(grepl(":", f2)){
        f2 = gsub(" ","", f2)
        start = substr(f2, 1, 4)
        end = substr(f2, 6, 9)
        f2 = seq(as.numeric(start), as.numeric(end), 1)
      }
      f2_projects = projects[as.character(projects$proj_year) %in% f2,]
      f2_projects = f2_projects[order(f2_projects$proj_name),]
      rm(f2)
    }# end of filter 2
    
    
    # filter 3, keyword ----
    if("Keywords" %in% selFilter){
      keywords_db = DBI::dbReadTable(db_con, DBI::Id(schema = "projects", table = "keywords"))
      keywords_db = keywords_db[order(keywords_db$keyword),]
      message("Filter by project keywords.")
      writeLines("Select keywords:")
      f3 = utils::select.list(sort(keywords_db$keyword), graphics = F, multiple = T)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f3)){
        q_i = paste0("'", paste0("%", f3[i], "%"), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      f3_projects <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year,
         pri.proj_description
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         left join 
         projects.keywords k on k.keyword_id = prk.keyword_id
         where keyword like any (array[", q_1,"])"))
      f3_projects = f3_projects[order(f3_projects$proj_name),]
      rm(keywords_db, f3, q_1)
    }# end of filter 3
    
    
    # filter 4, participants ----
    if("Persons" %in% selFilter){
      persons_db = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "people"))
      persons_db = persons_db[order(persons_db$family_name),]
      message("Filter by project participants")
      writeLines("In order to restrict names search you can filter for parts or first letters of family names  (e.g. 'ax' for 'Axtner'.\nIf you don want to restrict names search at all just press enter.")
      p_filter = readline("Enter part of family name: ")
      f_people = persons_db[grepl(tolower(p_filter), tolower(paste(persons_db$family_name, persons_db$first_name, sep=", "))),]
      
      name_width1 <- max(nchar(f_people$first_name)) + 2
      name_width2 <- max(nchar(f_people$family_name)) + 2
      writeLines("Select person(s) from the list:")
      persons <- utils::select.list(sprintf(paste0("%-", name_width2, "s %", name_width1,"s %4s"), f_people$family_name, f_people$first_name, f_people$people_id), multiple = T, graphics = F)
      f4 <- sub(".*\\s(\\d+)$", "\\1", persons)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f4)){
        q_i = paste0("'", paste0(f4[i]), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      f4_projects <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year,
         pri.proj_description
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         left join projects.proj_people prp on prp.proj_id = pri.proj_id
         left join people.people pep on pep.people_id = prp.people_id
         where prp.people_id in (",q_1,")"
      ))
      f4_projects = f4_projects[order(f4_projects$proj_name),]
      rm(persons_db, f4, q_1, f_people, name_width1, name_width2)
    }# end of filter 4
    
    
    # filter 5, organisation ----
    if("Organisations" %in% selFilter){
      message("Filter by project organisations.")
      writeLines("Select organisation from the list:")
      orgs_db = DBI::dbReadTable(db_con, DBI::Id(schema = "people", table = "organisations"))
      orgs_db = orgs_db[order(orgs_db$organisation),]
      name_width1 <- max(nchar(orgs_db$organisation)) + 2
      name_width2 <- max(nchar(orgs_db$abbreviation)) + 2
      writeLines("Select organisations related to the project:")
      orgs <- utils::select.list(sprintf(paste0("%-", name_width1, "s %", name_width2,"s %4s"), orgs_db$organisation, orgs_db$abbreviation, orgs_db$org_id), multiple = T, graphics = F)
      f5 <- sub(".*\\s(\\d+)$", "\\1", orgs)
      # create query string q_1 for more flexible LIKE ANY query in combination with ARRAY
      for(i in 1 : length(f5)){
        q_i = paste0("'", paste0(f5[i]), "'")
        if(i == 1){
          q_1 = q_i
        }
        if(i > 1){
          q_1 = paste(q_1, q_i, sep = ", ")
        }
        rm(q_i)
      }
      f5_projects <- DBI::dbGetQuery(db_con, paste0(
        "select distinct
         pri.proj_id, 
         proj_name, 
         proj_year,
         pri.proj_description
         from projects.proj_info pri
         left join 
         projects.proj_keywords prk on prk.proj_id = pri.proj_id
         left join projects.proj_organisations pro on pro.proj_id = pri.proj_id
         left join people.organisations peo on peo.org_id = pro.org_id
         where pro.org_id in (",q_1,")"
      ))
      f5_projects = f5_projects[order(f5_projects$proj_name),]
      rm(orgs_db, f5, q_1, name_width1, name_width2)
    }# end of filter 5
    
    # prompt result of filter to choose from ----
    f_projects = unique(rbind(if(exists("f1_projects")){f1_projects}, if(exists("f2_projects")){f2_projects}, if(exists("f3_projects")){f3_projects}, if(exists("f4_projects")){f4_projects}, if(exists("f5_projects")){f5_projects}))
    
    if(selFilter == "No filter"){
      f_projects = projects
    }
    
    f_projects = f_projects[order(f_projects$proj_name),]
    
    if(exists("f1_projects")){f_projects$name.search = ifelse(f_projects$proj_id %in% f1_projects$proj_id, "x", "0")}
    
    if(exists("f2_projects")){f_projects$year.search = ifelse(f_projects$proj_id %in% f2_projects$proj_id, "x", "0")}
    
    if(exists("f3_projects")){f_projects$keyword.search = ifelse(f_projects$proj_id %in% f3_projects$proj_id, "x", "0")}
    
    if(exists("f4_projects")){f_projects$people.search = ifelse(f_projects$proj_id %in% f4_projects$proj_id, "x", "0")}
    
    if(exists("f5_projects")){f_projects$org.search = ifelse(f_projects$proj_id %in% f5_projects$proj_id, "x", "0")}

    if(exists("f_projects") & is.null(f_projects)){
      message("Sorry, no project found that matches your parameters.\nYou want to try it again and refine your search?")
      again = utils::select.list(c("Yes", "No"), graphics=F)
      if(again == "Yes"){filter()} else {message("\nYou decided to leave. Have a nice day!")}
    } else {
      name_width = max(nchar(f_projects$proj_name)) + 2
      writeLines("\nAfter filtering the follwoing projects remain:")
      print(f_projects[, !(names(f_projects) == "proj_description")])
      writeLines("\nPlease select project(s) from the list below.")
      project <<- utils::select.list(sprintf(paste0("%-", name_width, "s %6s %4s"), f_projects$proj_name, f_projects$proj_year, f_projects$proj_id), multiple = T, graphics = F)
      
      matches <- regexec("^(.*)\\s+(\\d{4})\\s+(\\d+)$", project)
      parts <- regmatches(project, matches)
      proj_name  <<- trimws(sapply(parts, `[`, 2))
      proj_year  <<- as.numeric(sapply(parts, `[`, 3))
      proj_id <<- as.numeric(sapply(parts, `[`, 4))
    }
    
  }# end of filter()
  
  # run filter() function
  filter()
  
}