library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(dplyr)

# load data
programs <- read.csv("SampleDjangoAppData_2021q1 - RShiny_HybridTable_sample_geocoded.csv")

# UI setup ----
ui <- shinyUI(fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("MATchMapper"),
  tags$h4("A search tool for medication-assisted treatment (MAT) in Philadelphia"),
  # tags$h5(tagList("Data Source:", a("SAMHSA", href = "https://findtreatment.samhsa.gov/locator"))),
  tags$h5(tagList("Code:", a("Github", href = "https://github.com/dbowden/MATapp"))),
  sidebarLayout(
    sidebarPanel(width = 3,
                 tags$b("Data Sources"),
                 checkboxInput("Phila.TAD", "Philadelphia DBHIDS", value=F),
                 checkboxInput("FQHC", "HRSA + HFP: Federally Qualified Health Centers", value=F),
                 tags$small("SAMHSA:"),
                 checkboxInput("FT.Loc", "Find Treatment Locator", value=F),
                 checkboxInput("OTP.Dir", "Opioid Treatment Programs", value=F),
                 checkboxInput("Other.Src", "OTHER", value=F),
                 tags$b("Medications Prescribed"),
                 checkboxInput("bup", "Buprenorphine", value=F),
                 checkboxInput("meth", "Methadone", value=F),
                 checkboxInput("nal", "Naltrexone", value=F),
                 tags$b("Also Available"),
                 checkboxInput("primary", "Primary Care", value=F),
                 checkboxInput("tele", "Telehelth", value=F),
                 checkboxInput("pay", "Payment Assistance", value=F),
                 checkboxInput("medicaid", "Accepts Medicaid", value=F),
                 tags$b("Map Options"),
                 checkboxInput("cluster", "Group Overlapping Points", value=F),
                 checkboxInput("unclear", "Omit Unclear Results", value=F)
    ),
    mainPanel(
      leafletOutput("leafletmap", width = "600px"),
      dataTableOutput("tbl", width = "600px")
    )
  ),
  # hr(),
))


# Server setup ----
server <- function(input, output) {
  
  # set initial bounding box for map
  in_bounding_box <- function(data, latitude, longitude, bounds) {
    data %>%
      filter(
        latitude > bounds$south &
          latitude < bounds$north &
          longitude < bounds$east & longitude > bounds$west)
  }
  
  # Reactives ----
  
  # create function for chaining filters
  conditional <- function(condition, filter){
    if (condition) filter else TRUE
  }
  
  # create filtered data
  map_data_react <- reactive({
    
    if (input$unclear == TRUE){
      
      programs %>%
        filter(
          conditional(input$Phila.TAD == TRUE, Phila.TAD == TRUE),
          conditional(input$FQHC == TRUE, FQHC == TRUE),
          conditional(input$FT.Loc == TRUE, FT.Loc == TRUE),
          conditional(input$OTP.Dir == TRUE, OTP.Dir == TRUE),
          conditional(input$Other.Src == TRUE, Other.Src == TRUE),
          conditional(input$bup == TRUE, Buprenorphine == "Yes"),
          conditional(input$meth == TRUE, Methadone == "Yes"),
          conditional(input$nal == TRUE, Naltrexone == "Yes"),
          conditional(input$primary == TRUE, Primary.Care == "Yes"),
          conditional(input$tele == TRUE, Telehealth == "Yes"),
          conditional(input$pay == TRUE, Payment.Assistance == "Yes"),
          conditional(input$medicaid == TRUE, Medicaid == "Yes"),
        )
      
    } else {
      
      programs %>%
        filter(
          conditional(input$Phila.TAD == TRUE, Phila.TAD == TRUE),
          conditional(input$FQHC == TRUE, FQHC == TRUE),
          conditional(input$FT.Loc == TRUE, FT.Loc == TRUE),
          conditional(input$OTP.Dir == TRUE, OTP.Dir == TRUE),
          conditional(input$Other.Src == TRUE, Other.Src == TRUE),
          conditional(input$bup == TRUE, Buprenorphine %in% c("Yes", "Unclear")),
          conditional(input$meth == TRUE, Methadone  %in% c("Yes", "Unclear")),
          conditional(input$nal == TRUE, Naltrexone  %in% c("Yes", "Unclear")),
          conditional(input$primary == TRUE, Primary.Care  %in% c("Yes", "Unclear")),
          conditional(input$tele == TRUE, Telehealth  %in% c("Yes", "Unclear")),
          conditional(input$pay == TRUE, Payment.Assistance  %in% c("Yes", "Unclear")),
          conditional(input$medicaid == TRUE, Medicaid  %in% c("Yes", "Unclear")),
        )
      
    }
    
    
    
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
        ~ longitude ,
        ~ latitude,
        popup = ~ paste("<b>",System.Name,"</b>","<br>",Address,"<br>","Philadelphia, PA ",ZIP,"<br>",Phone,"<br>", paste0('<a href=','"',Website,'"','>', Website,'</a>',sep="")),
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
      in_bounding_box(programs, latitude, longitude, bounds)
    }
  })
  
  # Table output ----
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      # data_map() %>% select(Name = System.Name, Address = Address, Phone = Phone, Website = Website, Buprenorphine = Buprenorphine),
      map_data_react() %>% select(Name = System.Name, Address = Address, Phone = Phone, Website = Website),
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