#' view points of projects of the EcoDyn database
#' 
#' viewPoints 
#' 
#' Function to view geometric points of projects of the EcoDyn database.
#' 
#' @export


showPoints = function(){
  message("Welcome, you map point of one or more existing projects of the EcoDynDB.\nStep 1: Select project(s):")
  
  # select project
  RHelperEcoDynDB:::.selectProject()
  
  for(i in 1 : length(proj_id)){
    q_i = paste0("'", proj_id[i], "'")
    if(i == 1){
      q_1 = q_i
    }
    if(i > 1){
      q_1 = paste(q_1, q_i, sep = ", ")
    }
  }
  
  points = sf::st_read(dsn = db_con, query = paste0(
    "SELECT ps.proj_id, pi.proj_name, pn.point_name, pn.p_name_id, pg.p_geo_id, pg.geom FROM geodata.point_names pn JOIN projects.proj_sites ps ON ps.p_name_id = pn.p_name_id LEFT JOIN geodata.point_geometries pg ON pg.p_geo_id = pn.p_geo_id LEFT JOIN projects.proj_info pi ON pi.proj_id = ps.proj_id WHERE ps.proj_id IN (", q_1, ")"
  ))
  
  mapview::mapview(unique(points), zcol= "proj_name", legend=T, col.regions = viridisLite::viridis)
}
