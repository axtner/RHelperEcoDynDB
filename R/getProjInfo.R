#' Read project realted information from EcoDyn database
#' 
#' getProjInfo 
#' 
#' Function to reads information like persons, organisations, etc. associated with a certain project from the database.
#' 
#' @param db_user Database user. User need to provide credentials with the right permissions.
#' 
#' @export

getProjInfo = function(){
  
  # welcome message
  writeLines("\nWelcome!\nYou are searching for a special project of the EcoDyn database.\nYou can filter the search by \n(*)part of the project name, \n(*)start year of a project, \n(*)project keywords, \n(*)name of a project participant or \n(*)an involved organisation.")
  
  RHelperEcoDynDB:::.selectProject()
  
  # query project information from database
  # project people ----
  p_people <- DBI::dbGetQuery(db_con, paste0(
    "select distinct
     family_name, 
     first_name,
     proj_role
     from projects.proj_info pri
     left join projects.proj_people prp on prp.proj_id = pri.proj_id
     left join people.people pep on pep.people_id = prp.people_id
     where pri.proj_id = ", proj_id
  ))
  p_people <- p_people[order(p_people$family_name, p_people$first_name, p_people$proj_role),]
  if(!(is.na(p_people$family_name[1]))){
    p_people$full_name <- paste(p_people$family_name, p_people$first_name, sep = ", ")
    agg1 <- aggregate(proj_role ~ full_name, data = p_people, FUN = function(x) paste(x, collapse = ", "))
    name_width1 <- max(nchar(agg1$full_name)) + 2
  }
  
  # project organisations ----
  p_orgs <- DBI::dbGetQuery(db_con, paste0(
    "select distinct 
     organisation, 
     abbreviation,
     proj_org_role     
     from projects.proj_info pri
     left join projects.proj_organisations pro on pro.proj_id = pri.proj_id     
     left join people.organisations peo on peo.org_id = pro.org_id     
     where pri.proj_id = ", proj_id
  ))
  p_orgs <- p_orgs[order(p_orgs$organisation, p_orgs$abbreviation, p_orgs$proj_org_role),]
  if(!(is.na(p_orgs$organisation[1]))){
    p_orgs$org_name <- paste(p_orgs$organisation, p_orgs$abbreviation, sep = ", ")
    agg2 <- aggregate(proj_org_role ~ org_name, data = p_orgs, FUN = function(x) paste(x, collapse = ", "))
    name_width2 <- max(nchar(agg2$org_name)) + 2
  }
  
  # project keywords ----
  p_keys <- DBI::dbGetQuery(db_con, paste0(
    "select distinct
     keyword
     from projects.proj_info pri
     left join 
     projects.proj_keywords prk on prk.proj_id = pri.proj_id
     left join
     projects.keywords k on k.keyword_id = prk.keyword_id
     where pri.proj_id = ", proj_id
  ))
  
  
  writeLines(paste0("\nInformation on project '", projects$proj_name[projects$proj_id == proj_id], "'"))
  writeLines(paste0("\nStart year: ", projects$proj_year[projects$proj_id == proj_id]))
  writeLines("\nPersons involved in the project and their role:")
  if(!(is.na(p_people$family_name[1]))){
    writeLines(sprintf(paste0("%-", name_width1, "s %4s"), agg1$full_name, agg1$proj_role))
  } else {
    writeLines("No persons documented in the EcoDynDB for this project.")
  }
  writeLines("\nOrganisations involved in the project and their role:")
  if(!(is.na(p_orgs$organisation[1]))){
    writeLines(sprintf(paste0("%-", name_width2, "s %4s"), agg2$org_name, agg2$proj_org_role))
  } else {
    writeLines("No organistions documented in the EcoDynDB for this project.")
  }
  writeLines("\nKeywords associated with this project:")
  if(nrow(p_keys) != 0) {
    writeLines(paste(p_keys$keyword))
  } else {
    writeLines("No keywords documented in the EcoDynDB for this project.")
  }
  writeLines("\nProject description:")
  writeLines(paste(projects$proj_description[projects$proj_id == proj_id]))
  
  
}# end of getProjInfo()
