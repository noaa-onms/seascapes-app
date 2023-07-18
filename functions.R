get_sanctuary_ply <- function(
    sanctuary,
    dir_ply){
  
  if (length(strsplit(sanctuary, "-")[[1]]) > 1){
    
    # get mbnms-david or mbnms-main
    ply_geo <- glue("{dir_ply}/{sanctuary}.geojson")
    ply <- read_sf(ply_geo)
    
  } else {
    
    ply <- get_url_ply(
      sanctuary = sanctuary,
      dir_ply   = dir_ply)
    
  }
  
  ply
}