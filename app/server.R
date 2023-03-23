function(input, output, session) {

  values <- reactiveValues(
    date      = date_end,
    tbl       = tbl_1,
    sanctuary = sanctuary_1,
    ply       = ply_1,
    pal       = pal_1,
    col       = col_1)
  
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
      values$col       <- values$pal(attr(values$pal, "classes"))
      values$grd       <- get_grd(values$sanctuary, values$date)
    })
  
  # map ----
  output$map <- renderLeaflet({
    # vary this map with sanctuary only, so using date_end
    #   use proxy for varying date so doesn't zoom out
    sanctuary <- input$selSanctuary
    
    ply <- get_url_ply(sanctuary, dir_ply = dir_ply)
    grd <- get_grd(sanctuary, date_end)
    tbl <- get_ts(sanctuary)
    pal <- get_pal(tbl)
    classes <- attr(pal, "classes")
    
    suppressWarnings({
      m <- leaflet() %>%
        # add base: blue bathymetry and light brown/green topography
        addProviderTiles(
          "Esri.OceanBasemap",
          options = providerTileOptions(
            variant = "Ocean/World_Ocean_Base",
            opacity = 0.6)) %>%        
        # add reference: placename labels and borders
        addProviderTiles(
          "Esri.OceanBasemap",
          options = providerTileOptions(
            variant = "Ocean/World_Ocean_Reference",
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
          html     = date_end) %>% 
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
    
    g <- plot_ss_ts(values$tbl, values$col)

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
