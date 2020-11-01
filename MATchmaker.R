library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(dplyr)

# load data
programs <- read.csv("dbhids_geocoded.csv")
programs <- subset(programs, select=-c(X))

# UI setup ----
ui <- shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("MATchMapper"),
  tags$h4("A search tool for medication-assisted treatment (MAT) in Philadelphia"),
  tags$h5(tagList("Data Source:", a("DBHIDS Treatment Availability Database", href = "https://dbhids.org/MAT"))),
  tags$h5(tagList("Code:", a("Github", href = "https://github.com/dbowden/MATapp"))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 tags$b("Treatments Available"),
                 checkboxInput("bup", "Buprenorphine", value=F),
                 checkboxInput("meth", "Methadone", value=F),
                 checkboxInput("viv", "Vivitrol", value=F),
                 tags$b("Other Features"),
                 checkboxInput("same", "Same-Day Induction", value=F),
                 tags$b("Map Options"),
                 checkboxInput("cluster", "Group Overlapping Points", value=F)
    ),
    mainPanel(
      leafletOutput("leafletmap", width = "600px"),
      dataTableOutput("tbl", width = "600px")
    )
  ),
  hr(),
  # print("Source: Philadelphia Department of Behavioral Health and Intellectual disAbility Services")
))


# Server setup ----
server <- function(input, output) {
  
  # set initial bounding box for map
  in_bounding_box <- function(data, lat, lon, bounds) {
    data %>%
      filter(
        lat > bounds$south &
          lat < bounds$north &
          lon < bounds$east & lon > bounds$west)
  }
  
  # Reactives ----
  
  # create function for filtering data
  conditional <- function(condition, success){
    if (condition) success else TRUE
  }
  
  # create filtered data
  map_data_react <- reactive({
    
    programs %>% 
      filter(
        conditional(input$bup == TRUE, Buprenorphine. == "Yes"),
        conditional(input$meth == TRUE, Methadone. == "Yes"),
        conditional(input$viv == TRUE, Vivitrol. == "Yes"),
        conditional(input$same == TRUE, Same.day.induction.during.walk.in.hours. == "Yes"))
    
  })
  
  # create function to toggle clustering
  toggle_react <- reactive({
    if (input$cluster){
      return(markerClusterOptions())
    } else {
      return(NULL)
    }
  })
  
  # Map output ----
  output$leafletmap <- renderLeaflet({
    
    program_data <- map_data_react
    
    program_data %>% leaflet() %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addCircleMarkers(
        data = map_data_react(),
        ~ lon ,
        ~ lat,
        popup = ~ paste("<b>",ProgramName,"</b>","<br>",Address,"<br>","Philadelphia, PA ",Zip,"<br>",Phone,"<br>", paste('<a href=','"',Website,'"','>', Website,'</a>',sep="")),
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.4,
        popupOptions = popupOptions(closeButton = FALSE),
        clusterOptions = toggle_react()
      )
  })
  
  # make table show only datapoints visible on map
  data_map <- reactive({
    if (is.null(input$leafletmap_bounds)) {
      map_data_react()
    } else {
      bounds <- input$leafletmap_bounds
      in_bounding_box(programs, lat, lon, bounds)
    }
  })
  
  # Table output ----
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      data_map() %>% select(-lon, -lat),
      # map_data_react() %>% select(-lon, -lat),
      extensions = "Scroller",
      style = "bootstrap",
      class = "compact",
      width = "100%",
      options = list(
        deferRender = TRUE,
        scrollY = 300,
        scrollX = TRUE,
        scroller = TRUE,
        dom = 'tp'
      )
    )
  })
  
  
}

# run ----
shinyApp(ui = ui, server = server)