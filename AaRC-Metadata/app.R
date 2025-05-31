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
      tags$img(src = "AaRC_logo.png", height = "40px", style = "margin-right: 0px;"),
      tags$div("aDNA Metadata",
               style = "font-size: 20px; text-align: center; color: black; font-weight: normal;"
      )
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
      .scroll-container {
        overflow-x: auto;
        overflow-y: hidden;
        width: 100%;
        padding-bottom: 10px;
      }
    "))
  ),
  
  tabPanel("Explore",
           div(class = "container-custom",
               fluidRow(
                 column(4,
                        pickerInput("species", "Species",
                                    choices = unique(df$samp_taxon_common),
                                    selected = unique(df$samp_taxon_common),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        
                        pickerInput("sex", "Sex",
                                    choices = unique(df$molecular_sex),
                                    selected = unique(df$molecular_sex),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        
                        pickerInput("platform", "Sequencing Platform",
                                    choices = unique(df$nuclear_sequencing_platform),
                                    selected = unique(df$nuclear_sequencing_platform),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        
                        pickerInput("strandedness", "Library Strandedness",
                                    choices = unique(df$nuclear_lib_strandedness),
                                    selected = unique(df$nuclear_lib_strandedness),
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE)),
                        
                        sliderInput("age", "Sample Age Range (Years BP)",
                                    min = 0,
                                    max = 150000,
                                    value = c(0, 150000)),
                        
                        sliderInput("coverage", "Nuclear Coverage",
                                    min = 0,
                                    max = 40,
                                    value = c(0, 40)),
                        
                        selectInput("colorBy", "Colour map points by:",
                                    choices = c("Species" = "samp_taxon_common",
                                                "Sex" = "molecular_sex",
                                                "Age" = "sample_age",
                                                "Coverage" = "nuclear_depth_of_coverage"),
                                    selected = "samp_taxon_common"),
                        
                        uiOutput("colorLegend")
                 ),
                 column(8,
                        leafletOutput("map", height = 600),
                        br(), br(),
                        div(style = "display: flex; justify-content: space-between; align-items: center;",
                            h4("Your filtered dataset:", style="font-size: 30px"),
                            downloadButton("downloadData", "Download CSV")
                        ),
                        div(class = "scroll-container",
                            dataTableOutput("table")
                        )
                 )
               )
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
               p("This database was curated by the following researchers."),
               tags$ul(
                 tags$li("Dr Anders Bergström – University of East Anglia"),
                 tags$li("Dr Kevin Daly – University College Dublin"),
                 tags$li("Róisín Ferguson – University of East Anglia"),
                 tags$li("Matthias Sherman – Francis Crick Institute")
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
        samp_taxon_common %in% input$species,
        molecular_sex %in% input$sex,
        nuclear_sequencing_platform %in% input$platform,
        nuclear_lib_strandedness %in% input$strandedness
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(df$longitude, na.rm = TRUE),
              lat = mean(df$latitude, na.rm = TRUE),
              zoom = 2)
  })
  
  observe({
    data <- filteredData()
    colorField <- input$colorBy
    
    if (colorField %in% c("samp_taxon_common", "molecular_sex")) {
      color_pal <- colorFactor(palette = "Set1", domain = unique(df[[colorField]]))
      colors <- color_pal(data[[colorField]])
    } else {
      color_pal <- colorNumeric(palette = "viridis", domain = df[[colorField]], na.color = "grey")
      colors <- color_pal(data[[colorField]])
    }
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste0(
          "<b>Sample:</b> ", samp_name, "<br>",
          "<b>Species:</b> ", samp_taxon_common, "<br>",
          "<b>Sex:</b> ", molecular_sex, "<br>",
          "<b>Age:</b> ", sample_age, "<br>",
          "<b>Coverage:</b> ", nuclear_depth_of_coverage, "<br>",
          "<b>Site:</b> ", site_name, "<br>",
          "<b>Location:</b> ", geo_loc_name
        ),
        radius = 7,
        fillColor = colors,
        color = "black",
        weight = 0.5,
        fillOpacity = 1
      )
  })
  
  output$colorLegend <- renderUI({
    colorField <- input$colorBy
    if (colorField %in% c("samp_taxon_common", "molecular_sex")) {
      vals <- unique(df[[colorField]])
      pal <- colorFactor("Set1", domain = vals)
      div(
        style = "margin-top: 10px;",
        strong("Legend:"),
        tags$ul(style = "list-style: none; padding-left: 0;",
                lapply(vals, function(val) {
                  tags$li(
                    tags$span(style = paste0("display:inline-block;width:12px;height:12px;background-color:",
                                             pal(val), ";margin-right:6px;")),
                    val
                  )
                })
        )
      )
    } else {
      pal <- colorNumeric("viridis", domain = df[[colorField]], na.color = "grey")
      minVal <- round(min(df[[colorField]], na.rm = TRUE), 2)
      maxVal <- round(max(df[[colorField]], na.rm = TRUE), 2)
      div(
        style = "margin-top: 10px;",
        strong("Legend:"),
        tags$div(style = "display: flex; align-items: center;",
                 tags$span(paste(minVal), style = "margin-right: 8px;"),
                 tags$div(style = paste0("height: 10px; width: 150px; background: linear-gradient(to right,",
                                         paste(sapply(seq(0, 1, length.out = 10),
                                                      function(x) pal(x * (maxVal - minVal) + minVal)),
                                               collapse = ","), "); margin: 0 8px;")),
                 tags$span(paste(maxVal))
        )
      )
    }
  })
  
  output$table <- renderDataTable({
    datatable(
      filteredData(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE
      ),
      class = "display nowrap"
    )
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