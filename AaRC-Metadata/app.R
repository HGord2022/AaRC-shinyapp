library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(shinyWidgets)

# Load data
df <- read.csv("aarc_metadata.csv", stringsAsFactors = FALSE)

# Clean and convert data
df$sample_age <- as.numeric(df$sample_age)
df$nuclear_depth_of_coverage <- as.numeric(df$nuclear_depth_of_coverage)
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)
df <- df %>% filter(!is.na(latitude), !is.na(longitude))

ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center; padding-left: 10px;",
    div(
      tags$img(src = "AaRC_logo.png", height = "50px", style = "margin-right: 10px;"),
      tags$div("Metadata", style = "font-size: 14px; text-align: center;")
    )
  ),
  
  header = tags$head(
    tags$style(HTML("
      .navbar {
        min-height: 80px;
      }
      .navbar-brand {
        height: 80px;
        padding-top: 10px;
        padding-bottom: 10px;
      }
      .navbar-nav > li > a {
        padding-top: 30px;
        padding-bottom: 30px;
      }
      .container-custom {
        max-width: 1200px;
        margin: 0 auto;
      }
    "))
  ),
  
  tabPanel("Explore",
           div(class = "container-custom",
               leafletOutput("map", height = 600),
               br(),
               fluidRow(
                 column(4,
                        pickerInput("species", "Select Species",
                                    choices = unique(df$samp_taxon_common),
                                    selected = unique(df$samp_taxon_common),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE))
                 ),
                 column(4,
                        sliderInput("age", "Sample Age Range (Years BP)",
                                    min = min(df$sample_age, na.rm = TRUE),
                                    max = max(df$sample_age, na.rm = TRUE),
                                    value = c(min(df$sample_age, na.rm = TRUE), max(df$sample_age, na.rm = TRUE)))
                 ),
                 column(3,
                        sliderInput("coverage", "Nuclear Coverage",
                                    min = 0,
                                    max = max(df$nuclear_depth_of_coverage, na.rm = TRUE),
                                    value = c(0, max(df$nuclear_depth_of_coverage, na.rm = TRUE)))
                 ),
                 column(1,
                        br(),
                        downloadButton("downloadData", "Download CSV")
                 )
               ),
               br(),
               dataTableOutput("table")
           )
  ),
  
  tabPanel("About",
           div(class = "container-custom",
               h2("About the AaRC Metadata Project"),
               p("The AaRC (Ancient aDNA Research Community) Metadata Project aims to create a harmonised database compiling metadata for published ancient animal genomes."),
               p("We hope this will encourage better and more uniform metadata reporting standards. We eventually aim to align animal studies with other ongoing developments in the aDNA world, including humans."),
               p("This app is intended to allow people to explore what aDNA is out there, and provide access to the original publications and sequences.")
           )
  ),
  
  tabPanel("Curators",
           div(class = "container-custom",
               h2("Curators & Contributors"),
               p("This database was conceived and curated by the following researchers."),
               tags$ul(
                 tags$li("Dr Anders Bergstrom – University of East Anglia"),
                 tags$li("Dr Kevin Daly – University College Dublin"),
                 tags$li("Matthias Sherman - Francis Crick Institute")
               )
           )
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    df %>%
      filter(
        sample_age >= input$age[1],
        sample_age <= input$age[2],
        nuclear_depth_of_coverage >= input$coverage[1],
        nuclear_depth_of_coverage <= input$coverage[2],
        samp_taxon_common %in% input$species
      )
  })
  
  # Color palette based on species
  species_palette <- colorFactor(
    palette = "Set1",
    domain = unique(df$samp_taxon_common)
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(df$longitude, na.rm = TRUE),
              lat = mean(df$latitude, na.rm = TRUE),
              zoom = 2)
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste0(
          "<b>Sample:</b> ", samp_name, "<br>",
          "<b>Species:</b> ", samp_taxon_common, "<br>",
          "<b>Age:</b> ", sample_age, "<br>",
          "<b>Coverage:</b> ", nuclear_depth_of_coverage, "<br>",
          "<b>Site:</b> ", site_name, "<br>",
          "<b>Location:</b> ", geo_loc_name
        ),
        radius = 7,
        fillColor = ~species_palette(samp_taxon_common),
        color = "black",
        weight = 0.5,
        fillOpacity = 1
      )
  })
  
  output$table <- renderDataTable({
    filteredData() %>%
      select(samp_name, samp_taxon_common, sample_age, nuclear_depth_of_coverage, site_name, geo_loc_name, latitude, longitude)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("AaRC_filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
