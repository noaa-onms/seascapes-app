fluidPage(
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