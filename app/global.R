if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  bslib,
  dygraphs,
  glue,
  fs,
  here,
  kableExtra,
  leaflet,
  lubridate,
  marinebon/seascapeR, 
  raster,
  readr,
  shiny,
  stringr)
# Note: joshuaulrich/xts needed over CRAN xts for plot_ts_ss() with dygraphs(retainDateWindow = T) to work

# remotes::install_github("marinebon/seascapeR")
# devtools::load_all("~/github/seascapeR")
# devtools::install_local("~/github/seascapeR", force = T)

# TODO:
# - download grids (*.tif), time series (*.csv)

sanctuaries           <- nms
sanctuaries[["pmnm"]] <- NULL
sanctuaries           <- setNames(
  names(sanctuaries), sanctuaries)

dir_data <- here("data")
dir_ply  <- glue("{dir_data}/ply")
dir_grd  <- glue("{dir_data}/grd")

ss_dataset <- "global_monthly" # TODO: + "global_8day"
ss_var     <- "CLASS"          # TODO: + "P"

ts_csv <- dir_ls(dir_grd, regexp = fixed(glue("{ss_dataset}_{ss_var}.csv")), recurse = T)[1]
ts <- read_csv(ts_csv, col_types = cols())
date_beg  = min(ts$date)
date_end  = max(ts$date)
message(glue("date_beg - date_end: {date_beg} - {date_end}"))

get_ts_csv <- function(sanctuary){
  glue::glue(
    "{dir_grd}/{sanctuary}/{ss_dataset}_{ss_var}.csv")
}

get_ts_csv_out <- function(sanctuary){
  glue::glue(
    "{sanctuary}_{ss_dataset}_{ss_var}.csv")
}

get_ts <- function(sanctuary){
  ts_csv <- get_ts_csv(sanctuary)
  read_csv(ts_csv, col_types = cols()) 
}

get_pal <- function(tbl){
  classes <- unique(tbl$cellvalue) |> sort()
  
  pal <- colorFactor(
    "Spectral", 
    classes,
    na.color = NA,
    reverse = T)
  
  attr(pal, "classes") <- classes
  pal
}

get_grd_tif <- function(sanctuary, date){
  y <- year(date)
  m <- str_pad(month(date), 2, pad = "0")   
  
  glue(
    "{dir_grd}/{sanctuary}/{ss_dataset}/grd_CLASS_{y}.{m}.15.tif")
}

get_grd_tif_out <- function(sanctuary, date){
  y <- year(date)
  m <- str_pad(month(date), 2, pad = "0")   
  
  glue(
    "{sanctuary}_{ss_dataset}_CLASS_{y}.{m}.15.tif")
}

get_grds_zip <- function(sanctuary){
  glue(
    "{dir_grd}/{sanctuary}/{ss_dataset}.zip")
}

get_grds_zip_out <- function(sanctuary){
  glue(
    "{sanctuary}_{ss_dataset}.zip")
}

get_grd <- function(sanctuary, date){
  
  tif <- get_grd_tif(sanctuary, date)
  grd <- raster::raster(tif)
  names(grd) <- stringr::str_replace(names(grd), "^grd_", "")
  grd
}

sanctuary_1 <- sanctuaries[[1]]
ply_1       <- get_url_ply(sanctuary_1, dir_ply = dir_ply)
tbl_1       <- get_ts(sanctuary_1)
pal_1       <- get_pal(tbl_1)
col_1       <- pal_1(attr(pal_1, "classes"))
  
show_pal <- function(pal, vals){
  scales::show_col(pal(vals), labels=F, ncol = 1)
  colours <- matrix(pal(vals), ncol = 1, byrow = TRUE)
  text(col(colours) - 0.5, -row(colours) + 0.5, vals)
}
# show_pal(pal_1, attr(pal_1, "classes"))

# devtools::load_all(here("../../marinebon/seascapeR"))
# plot_ss_ts(tbl_1, col_1)

light <- bs_theme(
  version = 4,
  bootswatch = "slate",
  bg = "white",
  fg = "black")
dark <- bs_theme(
  version = 4,
  bootswatch = "slate",
  bg = "#272B30",
  fg = "#B9BEC2")
