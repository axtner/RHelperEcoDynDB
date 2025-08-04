#' get assignment results from the EcoDyn database
#' 
#' getAssResults 
#' 
#' Function to read assignment results from the EcoDyn database and save them as xlsx file.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

getAssResults <- function(){
  
  # check for database connection and connect if needed
  if(RHelperEcoDynDB::isEcoDynConnected() == FALSE){
    conn_test = FALSE
    RHelperEcoDynDB::EcoDynConnect()
  }
  
  if(exists("db_con", envir = .GlobalEnv) == T){
    db_con <- get("db_con", envir = .GlobalEnv)
  }
  
  # welcome message
  writeLines("\nWelcome!\nYou want to query assignment results from the EcoDyn database. \nYou will be able to filter the search by \n(*)project, \n(*)sample type, \n(*)study site, \n(*)taxonomic unit or \n(*)number of positive PCR replicates.\n\nYou can further decide to exclude all reads assigned to human or positive controls.\nResults will be exported as MS Excel sheet.")
  message("\nPlease note that the results you will receive are raw assignment results, without any editing.\nFinal acceptance as an assignment for a sample must always be made by careful visual inspection of the results.")  
  # select output directory
  writeLines("\nPlease select local output directory from the dialog window.")
  out_dir <<- utils::choose.dir(default = "Computer", "Select folder to save the assignment result file")
  file_name <<- paste0(out_dir, "\assignments_", format(Sys.time(),"%Y%m%d_%H%M%S"),".xlsx")
  
  # reading assignment results from database
  message("Running database query, please wait ...")
  assignments <- DBI::dbReadTable(db_con, DBI::Id(schema = "sfb", table = "assignment_results"))
  
  # filter functions ----
  filter <- function(){
    selFilter <<- utils::select.list(c("Project", "Sample type", "Study site", "Taxonomic unit", "Number of pos. PCR replicates", "No filter", "Exit"), title = "Please choose from the following options:", graphics=F, multiple = T)
    
    # filter1 for project ----
    if("Project" %in% selFilter){
      projects <- DBI::dbGetQuery(db_con, "SELECT * FROM projects.proj_info ORDER BY proj_name, proj_year")
      proj_ids = unique(assignments$proj_id)
      projects = projects[projects$proj_id %in% proj_ids,]
      name_width = max(nchar(projects$proj_name)) + 2
      project <<- utils::select.list(sprintf(paste0("%-", name_width, "s %6s %4s"), projects$proj_name, projects$proj_year, projects$proj_id), graphics = F, multiple = T)
      proj_id <<- sub(".*\\s(\\d+)$", "\\1", project)
      rm(proj_ids, projects, name_width)
    }# end of filter 1
    
    # filter2 for site ----
    if("Sample type" %in% selFilter){
      types <- unique(tolower(assignments$sample_type))
      sel_types <<-utils::select.list(types, graphics = F, multiple = T)
      rm(types)
    }# end of filter 2
    
    # filter3 for site ----
    if("Study site" %in% selFilter){
      sites <- unique(tolower(assignments$site))
      sel_sites <<-utils::select.list(sites, graphics = F, multiple = T)
      rm(sites)
    }# end of filter 4
    
    # filter4 for taxonomic unit ----
    if("Taxonomic unit" %in% selFilter){
      tlevel <<- utils::select.list(c("Species", "Genus", "Family"), title = "\nSelect at which taxonomic level you want to select a unit.", graphics = F)
      if(tlevel == "Species"){
        species = unique(sort(assignments$species))
        f_species = character(0)
        sel_species <<- character(0)
        select_sp = function(){
          while(length(f_species) == 0){
            presel = readline("Enter sci. species name (partly or first letter) to restrict search: ")
            f_species <<- species[grepl(tolower(presel), tolower(species))]
          } #end of while
          new_species <<- utils::select.list(f_species, graphics = F, multiple = F)
          sel_species <<- c(sel_species, new_species)
          again <<- utils::select.list(c("NO", "YES"), title = "\nYou want to select an additional species?")
          1}# end of select_sp()
        select_sp()
        while(again == "YES"){
          select_sp()
        }
        rm(presel, f_species, new_species, again)
      } # end of species level
      
      if(tlevel == "Genus"){
        genera = unique(sort(assignments$genus))
        f_genera = character(0)
        sel_genera <<- character(0)
        select_gen = function(){
          while(length(f_genera) == 0){
            presel = readline("Enter sci. genus name (partly or first letter) to restrict search: ")
            f_genera <<- genera[grepl(tolower(presel), tolower(genera))]
          } #end of while
          new_genus <<- utils::select.list(f_genera, graphics = F, multiple = F)
          sel_genera <<- c(sel_genera, new_genus)
          again <<- utils::select.list(c("NO", "YES"), title = "\nYou want to select an additional genus?")
          1}# end of select_gen()
        select_gen()
        while(again == "YES"){
          select_gen()
        }
        #rm(presel, f_genera, new_genera, again)
      } # end of genus level
      
      if(tlevel == "Family"){
        families = unique(sort(assignments$family))
        f_families = character(0)
        sel_families <<- character(0)
        select_fam = function(){
          while(length(f_families) == 0){
            presel = readline("Enter sci. family name (partly or first letter) to restrict search: ")
            f_families <<- families[grepl(tolower(presel), tolower(families))]
          } #end of while
          new_family <<- utils::select.list(f_families, graphics = F, multiple = F)
          sel_families <<- c(sel_families, new_family)
          again <<- utils::select.list(c("NO", "YES"), title = "\nYou want to select an additional family?")
          1}# end of select_gen()
        select_fam()
        while(again == "YES"){
          select_fam()
        }
        #rm(presel, f_families, new_family, again)
      } # end of family level
    }# end of filter 3
    
    # filter5 for number of positive PCRs ----
    if("Number of pos. PCR replicates" %in% selFilter){
      pos_pcrs <<- utils::select.list(c(1, 2, 3), title = "\nSelect number positive PCR replicates per sample you need for acceptance of an assignment", graphics = F)
    } #end of filter5
  } # end of filter()
  
  noisefilter <- function(){
    noise <<- utils::select.list(c("YES", "NO"), title = "You want to filter human and positive control?", graphics=F, multiple = F)
  } # end of noise filter
  
  # run filter() function
  filter()
  noisefilter()
  
  if("Exit" %in% selFilter){
    stop("You decided to stop by chosing 'Exit'. Have nice day!")
  }
  
  if("No filter" %in% selFilter){
    message("You chose 'No filter'. All other filter opions will be ignored")
  } else {
    
    if("Project name" %in% selFilter){
      writeLines("\nfiltering for project...")
      assignments <- assignments[assignments$proj_id %in% proj_id,]
      writeLines(paste0("nrow(assignments: ", nrow(assignments)))
    }
    
    if("Sample type" %in% selFilter){
      writeLines("\nfiltering for sample types...")
      assignments <- assignments[assignments$sample_type %in% sel_types,]
      writeLines(paste0("nrow(assignments: ", nrow(assignments)))
      if(nrow(assignments) == 0){
        stop(paste0("There are nor results for the selected sample type(s) '", paste0(sel_types, collapse="', '"), "'."))
      }
    }
    
    if("Study site" %in% selFilter){
      writeLines("\nfiltering for study sites...")
      assignments <- assignments[tolower(assignments$site) %in% sel_sites,]
      writeLines(paste0("nrow(assignments: ", nrow(assignments)))
      if(nrow(assignments) == 0){
        stop(paste0("\nThere are nor results for the selected study sites: '", paste0(sel_sites, collapse="', '")))
      }
    }
    
    if("Taxonomic unit" %in% selFilter){
      if(tlevel == "Species"){
        writeLines("\nfiltering for species...")
        assignments <- assignments[assignments$species %in% sel_species,]
        writeLines(paste0("nrow(assignments: ", nrow(assignments)))
        if(nrow(assignments) == 0){
          stop(paste0("\nThere are nor results for the selected species: ", paste0(sel_species, collapse="', '"),"'."))
        }
      }
      if(tlevel == "Genus"){
        writeLines("\nfiltering for genera...")
        assignments <- assignments[assignments$genus %in% sel_genera,]
        writeLines(paste0("nrow(assignments: ", nrow(assignments)))
        if(nrow(assignments) == 0){
          stop(paste0("\nThere are nor results for the selected gernera: ", paste0(sel_genera, collapse="', '"),"'."))
        }
      }
      if(tlevel == "Family"){
        writeLines("\nfiltering for families...")
        assignments <- assignments[assignments$family %in% sel_families,]
        writeLines(paste0("nrow(assignments: ", nrow(assignments)))
        if(nrow(assignments) == 0){
          stop(paste0("\nThere are nor results for the selected families: ", paste0(sel_families, collapse="', '"),"'."))
        }
      }
    }
    
    if("Number of pos. PCR replicates" %in% selFilter){
      writeLines("\nfiltering for PCR replicates...")
      assignments <<- assignments[assignments$pos_pcr_rep >= pos_pcrs,]
      writeLines(paste0("nrow(assignments: ", nrow(assignments)))
      if(nrow(assignments) == 0){
        stop(paste0("\nThere are nor results for the selected pPCR replicate threshold: '", pos_pcrs, "'."))
      }
    }
    
    if(noise == "YES"){
      writeLines("\nfiltering for noise...")
      assignments <- assignments[!(assignments$genus %in% c("Homo", "Myodes")),]
      writeLines(paste0("nrow(assignments: ", nrow(assignments)))
      if(nrow(assignments) == 0){
        stop("There are no results when you filter for human and positive control reads.")
      }
    }
  }
  
  file_name <- paste0(out_dir, "\\assignments_", format(Sys.time(),"%Y%m%d_%H%M%S"),".xlsx")
  openxlsx::write.xlsx(assignments, file_name, asTable = T)
  writeLines(paste0("\nThere were ", nrow(assignments), " assignments found."))
  writeLines(paste0("\nResults were written to file '", file_name, "'."))
  writeLines("\nHave a nice day!")
  

  } # end of function