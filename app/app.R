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

ss_info <- get_ss_info(ss_dataset)

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
  classes <- unique(tbl$cellvalue)
  
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

# ui ----
ui <- fluidPage(
  theme = dark,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(
    class = "custom-control custom-switch float-right", 
    tags$input(
      id = "dark_mode", type = "checkbox", checked = T, class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")),
    tags$label(
      "Dark Mode", `for` = "dark_mode", class = "custom-control-label")),
  div(
    class = "float-right", 
    a(href = "classes.html", "Classes", target="_blank"),
    HTML("&nbsp;&nbsp;"),
    actionLink("lnkAbout", HTML("About&nbsp;&nbsp;"))),
  titlePanel(
    tagList(
      HTML("<h2>
        <img src='logo.svg' align='left', height=50/> 
        &nbsp;Seascapes for Sanctuaries&nbsp;&nbsp;
        <span style='font-size:10px;'><a href='https://marinebon.org'>MarineBON.org</a></small>
        </h2>")),
    windowTitle = "Sanctuary Seascapes"),
  fluidRow(
    column(
      12,
      div(
        style = "padding-left: 100px; height: 0;", # 
        selectInput(
          "selSanctuary",
          "",
          sanctuaries)),
      leafletOutput("map", width = "100%", height = 350),
      fluidRow(
        column(
          12,
          "Download Sanctuary files:", 
          downloadLink("dlTif", "grid (.tif)"), ", ",
          downloadLink("dlZip", "all grids (.zip)"),  ", ",
          downloadLink("dlCsv", "timeseries (.csv)"))),
      sliderInput(
        "selDate", 
        "",
        min        = date_beg,
        max        = date_end,
        value      = date_beg, 
        step       = 10,
        animate    = T, 
        width      = "100%",
        timeFormat = "%Y-%m")),
    column(
      12,
      dygraphOutput("plot", width = "100%", height = 400))
  ))

server <- function(input, output, session) {

  values <- reactiveValues(
    date      = date_beg,
    tbl       = tbl_1,
    sanctuary = sanctuary_1,
    ply       = ply_1,
    pal       = pal_1)
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark else light
    )
  })
  observeEvent(
    input$selDate, {
      req(input$selSanctuary)
      
      values$date <- as.Date(glue("{year(input$selDate)}-{month(input$selDate)}-15"))
      
      values$grd  <- get_grd(values$sanctuary, values$date)
    })
  
  observeEvent(
    input$selSanctuary, {
      req(input$selDate)
      
      sanctuary <- input$selSanctuary
      tbl       <- get_ts(sanctuary)
      
      values$sanctuary <- sanctuary
      values$tbl       <- tbl
      values$ply       <- get_url_ply(sanctuary, dir_ply = dir_ply)
      values$pal       <- get_pal(tbl)
      
      values$grd       <- get_grd(values$sanctuary, values$date)
    })
  
  output$map <- renderLeaflet({
    # vary this map with sanctuary only, so using date_beg
    #   use proxy for varying date so doesn't zoom out
    sanctuary <- input$selSanctuary
    
    ply <- get_url_ply(sanctuary, dir_ply = dir_ply)
    grd <- get_grd(sanctuary, date_beg)
    tbl <- get_ts(sanctuary)
    pal <- get_pal(tbl)
    classes <- attr(pal, "classes")
    
    suppressWarnings({
      m <- leaflet() %>%
        addProviderTiles(
          providers$Esri.OceanBasemap,
          options = providerTileOptions(
            opacity = 0.6)) %>%
        addRasterImage(
          grd,
          project = T, method = "ngb",
          colors  = pal,
          opacity = 0.8,
          layerId = "grd",
          group = "Class") %>%
        addLegend(
          position = "bottomright",
          pal    = pal,
          values = classes,
          title  = "Class") %>%
        addPolygons(
          data        = ply,
          color       = "blue",
          fillOpacity = 0,
          group = "Sanctuary") %>% 
        addLayersControl(
          position = "topleft",
          overlayGroups = c("Class", "Sanctuary")) %>% 
        addControl(
          position = "bottomright",
          layerId  = "date",
          html     = date_beg) %>% 
        addMiniMap(
          position = "bottomleft",
          width    = 100,
          height   = 100,
          zoomLevelOffset = -7,
          toggleDisplay = T)
    })
    m
  })
  
  observe({
    proxy <- leafletProxy("map")
  
    sanctuary <- values$sanctuary
    date      <- values$date
    pal       <- values$pal
    classes   <- attr(pal, "classes")      
    grd       <- values$grd

    suppressWarnings({
      proxy <- proxy %>%
        clearImages() %>% 
        removeImage("grd") %>% 
        removeMarker("marker") %>% 
        removeControl("date") %>% 
        addRasterImage(
          grd,
          project = T, method = "ngb",
          colors  = pal,
          opacity = 0.8,
          layerId = "grd",
          group   = "Class") %>% 
        addControl(
          position = "bottomright",
          layerId  = "date",
          html     = date) })
    proxy
  })
  
  observe({
    req(input$map_click)
    
    ll  <- input$map_click
    grd <- values$grd
    pal <- values$pal
    
    xy <- data.frame(
      lon = ll$lng,
      lat = ll$lat)
    
    v <- try(raster::extract(grd, xy))
  
    if ("try-error" %in% class(v))
      browser()
    
    rround <- function(v) formatC(v, digits=3, format='f') %>% as.double()

    proxy <- leafletProxy("map")
    
    info <- HTML(glue(
      "
      <div class='info legend'>
        <div style='margin-bottom:3px'>
          <strong>Class</strong>
        </div>
        <i style='background:{pal(v)};opacity:0.5'></i>
        <b>{v}</b>
        <br>
        <small><em>
          {rround(xy$lon)}, {rround(xy$lat)}<br>
          {values$date}
        </small></em>
      </div>"))
    
    proxy %>% 
      removeMarker("marker") %>% 
      addLabelOnlyMarkers(
        lng     = xy$lon, 
        lat     = xy$lat,
        layerId = "marker",
        label   = info, 
        labelOptions = labelOptions(
          noHide = T))
  })
  
  # plot ----
  output$plot <- renderDygraph({
    
    g <- plot_ss_ts(values$tbl)

    if (isTRUE(input$dark_mode)){
      g <- g  %>%
        dyEvent(
          x        = values$date,
          label    = values$date,
          labelLoc = "bottom",
          color    = "white") %>%
        dyCSS("www/styles.css")
    } else {
      g <- g  %>%
        dyEvent(
          x        = values$date,
          label    = values$date,
          labelLoc = "bottom")
    }

    g
  })
  
  observe({
    date_click <- req(input$plot_click$x)
    
    updateSliderInput(
      session,
      "selDate",
      value = as.Date(date_click))
  })
  
  observeEvent(input$lnkAbout, {
    showModal(modalDialog(
      tags$head(
        tags$script(src="https://kit.fontawesome.com/fe79a3e7cf.js", crossorigin="anonymous")),
      h3(HTML("About <img src='logo.svg' align='right' height=100>")),
      readLines("about.md") %>% markdown(), 
      easyClose = TRUE,
      size = "l") # , footer = NULL
    )
  })
  
  output$dlTif <- downloadHandler(
    filename = function() {
      get_grd_tif_out(values$sanctuary, values$date) },
    content = function(file) {
      tif_in <- get_grd_tif(values$sanctuary, values$date)
      file.copy(tif_in, file) },
    #contentType = "application/zip"
    contentType = "image/tiff")
  
  output$dlZip <- downloadHandler(
    filename = function() {
      get_grds_zip_out(values$sanctuary) },
    content = function(file) {
      zip_in <- get_grds_zip(values$sanctuary)
      file.copy(zip_in, file) },
    contentType = "application/zip")
  
  output$dlCsv <- downloadHandler(
    filename = function() {
      get_ts_csv_out(values$sanctuary) },
    content = function(file) {
      csv_in <- get_ts_csv(values$sanctuary)
      file.copy(csv_in, file) },
    contentType = "text/csv")
}

shinyApp(ui = ui, server = server)
