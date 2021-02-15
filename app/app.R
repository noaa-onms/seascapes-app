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
  leaflet,
  lubridate,
  marinebon/seascapeR, 
  raster,
  readr,
  shiny,
  stringr)
# devtools::load_all("~/github/seascapeR")

# TODO:
# - download grids (*.tif), time series (*.csv)
# - seascapeR: update fxns to match colors b/n map and plot_ts

sanctuaries           <- nms
sanctuaries[["pmnm"]] <- NULL
sanctuaries           <- setNames(
  names(sanctuaries), sanctuaries)

dir_data <- here("data")
dir_ply  <- glue("{dir_data}/ply")
dir_grd  <- glue("{dir_data}/grd")

ss_dataset <- "global_monthly" # TODO: + "global_8day"
ss_var     <- "CLASS"          # TODO: + "P"

date_beg  = as.Date("2019-01-15")
date_end  = as.Date("2020-11-15")

ss_info <- get_ss_info(ss_dataset)

get_tbl <- function(sanctuary){
  ts_csv <- glue::glue(
    "{dir_grd}/{sanctuary}/{ss_dataset}_{ss_var}.csv")
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

get_grd <- function(sanctuary, date){
  y <- year(date)
  m <- str_pad(month(date), 2, pad = "0")   
  
  grd_tif <- glue(
    "{dir_grd}/{sanctuary}/{ss_dataset}/grd_CLASS_{y}.{m}.15.tif")

  grd <- raster::raster(grd_tif)
  names(grd) <- stringr::str_replace(names(grd), "^grd_", "")
  grd
}

sanctuary_1 <- sanctuaries[[1]]
ply_1       <- get_url_ply(sanctuary_1, dir_ply = dir_ply)
tbl_1       <- get_tbl(sanctuary_1)
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

ui <- fluidPage(
  theme = light,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")),
    tags$label(
      "Dark mode", `for` = "dark_mode", class = "custom-control-label")),
  
  titlePanel("Sanctuary Seascapes"),
  fluidRow(
    column(
      6,
      selectInput(
        "selSanctuary",
        "Sanctuary",
        sanctuaries),
      leafletOutput("map", width = "100%", height = 400),
      sliderInput(
        "selDate", 
        "Date",
        min        = date_beg,
        max        = date_end,
        value      = date_beg, 
        step       = 10,
        animate    = T, 
        width      = "100%",
        timeFormat = "%Y-%m")),
    column(
      6,
      dygraphOutput("plot", width = "100%", height = 550))
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
      tbl       <- get_tbl(sanctuary)
      
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
    
    # sanctuary = "nmsas"; date = as.Date("2019-01-15")
    ply <- get_url_ply(sanctuary, dir_ply = dir_ply)
    grd <- get_grd(sanctuary, date_beg)
    tbl <- get_tbl(sanctuary)
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
  
  output$plot <- renderDygraph({
    
    plot_ss_ts(values$tbl) %>% 
      dyEvent(
        x        = values$date, 
        label    = values$date, 
        labelLoc = "bottom")
    
  })
  
  output$clicked <- renderText({
    strftime(req(input$plot_click$x), "%Y-%m-%d")
  })
  
  observe({
    date_click <- req(input$plot_click$x)
    
    updateSliderInput(
      session,
      "selDate",
      value = as.Date(date_click))
  })
  
}

shinyApp(ui = ui, server = server)
