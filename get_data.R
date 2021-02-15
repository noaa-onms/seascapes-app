if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr,
  glue,
  here,
  marinebon/seascapeR)
# devtools::load_all("~/github/seascapeR")

# paths
dir_data = here("data")
dir_plys = glue("{dir_data}/ply")
dir_grds = glue("{dir_data}/grd")

sanctuaries = names(nms) %>% setdiff("pmnm")
# TODO: pmnm Error: 
#   One or both longitude values (-180, 180) outside data range (-179.975, 179.975)
  
# ss_datasets = c("global_monthly", "global_8day")
# ss_vars     = c("CLASS", "P")
# start simpler
ss_datasets = c("global_monthly")
ss_vars     = c("CLASS")
date_beg    = "2019-01-01"
date_end    = "2021-01-01"

msg_i <- function(name, itm, vec, show_time = T){
  i_vec <- which(itm == vec)
  s_time <- ifelse(show_time, glue(" - {Sys.time()}", ""))
  message(glue(
    "{name}: {itm} [{i_vec} of {length(vec)}]{s_time}"))
}

for (sanctuary in sanctuaries){ # sanctuary = sanctuaries[1]
  msg_i("sanctuary", sanctuary, sanctuaries)
  
  ply <- get_url_ply(
    sanctuary = sanctuary, 
    dir_ply   = dir_plys)
  
  for (ss_dataset in ss_datasets){ # ss_dataset = ss_datasets[1]
    msg_i("  dataset", ss_dataset, ss_datasets)
    
    ss_info <- get_ss_info(dataset = ss_dataset)
    
    dir_grd  = glue(
      "{dir_grds}/{sanctuary}/{ss_dataset}")
    
    for (ss_var in ss_vars){ # ss_var = ss_vars[1]
      msg_i("    var", ss_var, ss_vars)
      
      message("      get_ss_grds()")
      grds <- get_ss_grds(
        ss_info, ply, 
        date_beg = date_beg, 
        date_end = date_end,
        ss_var   = ss_var, 
        dir_tif  = dir_grd)
      
      if (ss_var == "CLASS"){
        ts_csv = glue(
          "{dir_grds}/{sanctuary}/{ss_dataset}_{ss_var}.csv")
        
        message("      sum_ss_grds_to_ts()")
        tbl <- sum_ss_grds_to_ts(grds, ts_csv = ts_csv)
      }
    }
  }
}
