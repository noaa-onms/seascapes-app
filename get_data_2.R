if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr,
  fs,
  glue,
  here,
  marinebon/seascapeR,
  stringr)
# devtools::install_local(here("../../marinebon/seascapeR"), force=T)
# devtools::load_all(here("../../marinebon/seascapeR"))

# paths
dir_data = here("data")
dir_plys = glue("{dir_data}/ply")
dir_grds = glue("{dir_data}/grd")

# sanctuaries = names(nms) %>% setdiff("pmnm")
# DEBUG
sanctuaries = names(nms) %>% setdiff(c("pmnm"))
rerddap::cache_delete_all(force = T)

# TODO: pmnm Error: 
#   One or both longitude values (-180, 180) outside data range (-179.975, 179.975)

ss_datasets <- c("global_monthly") # TODO: "global_8day"
ss_vars     <- c("CLASS")           # TODO: "P"
ss_info     <- get_ss_info(dataset = ss_datasets[1])

redo_ts    <- FALSE

msg_i <- function(name, itm, vec, show_time = T){
  i_vec <- which(itm == vec)
  s_time <- ifelse(show_time, glue(" - {Sys.time()}", ""))
  message(glue(
    "{name}: {itm} [{i_vec} of {length(vec)}]{s_time}"))
}

for (sanctuary in sanctuaries){ # sanctuary = "mbnms"  # nmsas "American Samoa"

  # nms = "mbnms"
  # nms = "mbnms-david"
  msg_i("sanctuary", sanctuary, sanctuaries)
  
  if (length(strsplit(nms, "-")[[1]]) > 1){
    ply_geo <- here(glue("data/ply/{nms}.geojson"))
    ply <- st_read(ply_geo)
  } else {
    ply <- get_url_ply(
      sanctuary = sanctuary,
      dir_ply   = dir_plys)
  }
  
  for (ss_dataset in ss_datasets){ # ss_dataset = ss_datasets[1]
    msg_i("  dataset", ss_dataset, ss_datasets)
    
    dir_grd  = glue(
      "{dir_grds}/{sanctuary}/{ss_dataset}")
    
    for (ss_var in ss_vars){ # ss_var = ss_vars[1]
      msg_i("    var", ss_var, ss_vars)

      message("      get_ss_grds()")
      grds <- get_ss_grds(
        ss_info, ply,
        ss_var   = ss_var,
        dir_tif  = dir_grd,
        verbose  = T)

      if (ss_var == "CLASS"){
        ts_csv = glue(
          "{dir_grds}/{sanctuary}/{ss_dataset}_{ss_var}.csv")
        
        if (redo_ts)
          file_delete(ts_csv)

        message("      sum_ss_grds_to_ts()")
        tbl <- sum_ss_grds_to_ts(grds, ts_csv = ts_csv, verbose = T)
        message("      end sum_ss_grds_to_ts()")
      }
      message("   end: ss_var in ss_vars")
    }
    message(" end: ss_dataset in ss_datasets")
    
    # TODO: breakup by ss_var
    # zip_f <- glue("{dir_grd}.zip")
    # zip(zip_f, dir_ls(dir_grd), flags = "-r9Xj")
  }
  message("end: sanctuary in sanctuaries")
}
