# rm(list = ls()); gc()

options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(bslib))
shhh(library(cptcity))
shhh(library(countrycode))
shhh(library(dplyr))
shhh(library(ggplot2))
shhh(library(lattice))
shhh(library(latticeExtra))
shhh(library(maps))
shhh(library(markdown))
shhh(library(purrr))
options("rgdal_show_exportToProj4_warnings"="none")
shhh(library(raster, warn.conflicts = FALSE)) # classes and functions for raster data
shhh(library(rasterVis))
shhh(library(readxl))
shhh(library(writexl))
shhh(library(rgdal, warn.conflicts = FALSE))
shhh(library(shiny))
shhh(library(shinyalert))
shhh(library(shinyvalidate))
shhh(library(shinyjs))
shhh(library(shinyhelper))
shhh(library(shinyWidgets))
shhh(library(sp))
shhh(library(sf))     # classes and functions for vector data
shhh(library(stringr))
shhh(library(terra, warn.conflicts=FALSE))  # suppressWarnings(suppressMessages(library(terra)))
shhh(library(tinytex))
shhh(library(shinythemes))
shhh(library(readxl))
shhh(library(dplyr))
shhh(library(leaflet))
shhh(library(leaflet.extras))
shhh(library(htmltools))
shhh(library(shinyBS))
shhh(library(plotly))
shhh(library(shinydashboard))
shhh(library(shinybusy))
shhh(library(DT))
shhh(library(zoo))
shhh((library(lubridate)))
shhh((library(rsconnect)))


############### Load data ###############
# setwd("C:/Users/ron18/Desktop/2022-1/bibs/covid19/kcovidtrack")

# Read data from CSV file
# data <- read.csv("Korea viz/data/covid_data.csv")
data_korea <- read.csv("Korea viz/data/data_korea.csv")
region_population <- read_excel("Korea viz/data/region_population.xlsx")
population <- read_excel("misc/population.xlsx", 1) #?
epiparms <- read_excel("misc/epiparms.xlsx", 1) #?
# region_df <- read.csv("Korea viz/data/region_data.csv")
full_region_df <- read.csv("Korea viz/data/region_data_05_31.csv")


# Read data for map
worldcountry = geojsonio::geojson_read("Korea viz/data/input_data/50m.geojson", what = "sp")
# cv_cases = read.csv("Korea viz/data/input_data/coronavirus.csv")
# countries = read.csv("Korea viz/data/input_data//countries_codes_and_coordinates.csv")
south_korea <- sf::st_read("Korea viz/data/south-korea-with-regions_1516.geojson") #region lon,lat
region_match <- read.csv("Korea viz/data/region_match.csv") # english/korean name

# setwd("C:/Users/ron18/Desktop/2022-1/bibs/covid19/kcovidtrack/")


############ data processing ###############

# select a month from region population
colnames(region_population)[1] <- 'region'
colnames(region_population)[2] <- 'population'
region_population <- subset(region_population, !(region %in% c("행정구역(시군구)별", "전국")))
region_population <- region_population[,1:2]
region_population$region <- region_match$eng[match(region_population$region, region_match$kor)]

# Remove total and lazaretto in region_df
# region_df <- subset(region_df, !(region %in% c("Total", "Lazaretto")))
full_region_df <- subset(full_region_df, !(region %in% c("Total", "Lazaretto")))

# Find indices of zero values in cum_deaths
zero_indices <- which(full_region_df$cum_deaths == 0)
# Find indices of zero values in cum_cases
zero_indices_cum_cases <- which(full_region_df$cum_cases == 0)
# Find indices of zero values in daily_cases
zero_indices_daily_case <- which(full_region_df$daily_cases == 0)

# Get the index of the start time point from where you want to interpolate (cum_deaths)
start_time_point <- 10000
# GET the same for cum_cases
start_time_point_cum_cases <- 10000
# GET the same for daily_cases
start_time_point_daily_cases <- 10000

# Filter out zero_indices that are greater than or equal to start_time_point
zero_indices_to_interpolate <- zero_indices[zero_indices >= start_time_point]
zero_indices_to_interpolate_daily_cases <- zero_indices_daily_case[zero_indices_daily_case >= start_time_point_daily_cases]
zero_indices_to_interpolate_cum_cases <- zero_indices_cum_cases[zero_indices_cum_cases >= start_time_point_cum_cases]



# Loop through the zero indices and replace with interpolated values
# cum deaths
for (i in zero_indices_to_interpolate) {
  prev_day <- max(which(full_region_df$cum_deaths[1:i] > 0))
  next_day <- min(which(full_region_df$cum_deaths[i:length(full_region_df$cum_deaths)] > 0))
  # Check if the previous and next day values are zero, and if so, skip interpolation
  if (full_region_df$cum_deaths[prev_day] == 0 && full_region_df$cum_deaths[next_day] == 0) {
    next
  }
  interpolated_value <- approx(x = c(prev_day, next_day), y = full_region_df$cum_deaths[c(prev_day, next_day)], xout = i)$y
  full_region_df$cum_deaths[i] <- interpolated_value
}
# target_date <- as.Date("2022-03-30")
# full_region_df <- full_region_df %>%
#   group_by(region) %>%
#   mutate(
#     cum_deaths = ifelse(
#       date == target_date,
#       round((cum_deaths[which(date == target_date) - 1] + cum_deaths[which(date == target_date) + 1]) / 2),
#       cum_deaths
#     )
#   ) %>%
#   ungroup()

# cum cases
for (i in zero_indices_to_interpolate_cum_cases) {
  prev_day <- max(which(full_region_df$cum_cases[1:i] > 0))
  next_day <- min(which(full_region_df$cum_cases[i:length(full_region_df$cum_cases)] > 0))
  # Check if the previous and next day values are zero, and if so, skip interpolation
  if (full_region_df$cum_cases[prev_day] == 0 && full_region_df$cum_cases[next_day] == 0) {
    next
  }
  interpolated_value <- approx(x = c(prev_day, next_day), y = full_region_df$cum_cases[c(prev_day, next_day)], xout = i)$y
  full_region_df$cum_cases[i] <- interpolated_value
}

# daily cases
for (i in zero_indices_to_interpolate_daily_cases) {
  prev_day <- max(which(full_region_df$daily_cases[1:i] > 0))
  next_day <- min(which(full_region_df$daily_cases[i:length(full_region_df$daily_cases)] > 0))
  # Check if the previous and next day values are zero, and if so, skip interpolation
  if (full_region_df$daily_cases[prev_day] == 0 && full_region_df$daily_cases[next_day] == 0) {
    next
  }
  interpolated_value <- approx(x = c(prev_day, next_day), y = full_region_df$daily_cases[c(prev_day, next_day)], xout = i)$y
  full_region_df$daily_cases[i] <- interpolated_value
}

# df <- full_region_df
# column_name <- "cum_deaths"
# date_to_interpolate <- "2022-03-30"

perform_interpolation <- function(df, date_to_interpolate) {
  regions <- unique(df$region)
  for (region in regions) {
    # region<- regions[1]
    region_data <- df[df$region == region, ]
    date_indices <- which(region_data$date == date_to_interpolate)
    
    if(length(date_indices) > 1) { # Only proceed if there are duplicates
      correct_index <- which.max(region_data$cum_cases[date_indices])
      incorrect_index <- which.min(region_data$cum_cases[date_indices])
      
      # Identify the corresponding full indices in df
      incorrect_rows <- which(df$region == region & df$date == date_to_interpolate)
      full_index_to_remove <- incorrect_rows[incorrect_index]
      
      # Remove the incorrect index from the main DataFrame
      df <- df[-full_index_to_remove, ]
    }
  }
  return(df)
}

date_to_interpolate <- "2022-03-30"
full_region_df <- perform_interpolation(full_region_df, date_to_interpolate)
date_to_interpolate <- "2022-03-22"
full_region_df <- perform_interpolation(full_region_df, date_to_interpolate)
date_to_interpolate <- "2022-03-25"
full_region_df <- perform_interpolation(full_region_df, date_to_interpolate)

# # Identify the indices for the date 2022-03-30
# dates_to_interpolate <- c("2022-03-30", "2022-03-22", "2022-03-25")
# indices_to_interpolate <- which(full_region_df$date %in% dates_to_interpolate)
# 
# # Apply interpolation for cum_deaths and cum_cases
# full_region_df$cum_deaths <- perform_interpolation(full_region_df$cum_deaths, which(full_region_df$date == "2022-03-30"))
# full_region_df$cum_cases <- perform_interpolation(full_region_df$cum_cases, which(full_region_df$date == "2022-03-30"))
# 
# # Apply interpolation for daily_cases for both dates
# full_region_df$daily_cases <- perform_interpolation(full_region_df$daily_cases, indices_to_interpolate)



south_korea_ch <- south_korea
south_korea_ch$name <- region_match$eng[match(south_korea_ch$name, region_match$kor)]


# Merge COVID-19 data with South Korea polygon data
# merged_data <- merge(south_korea_ch, region_df, by.x = "name", by.y = "region", all.x = TRUE)
full_merged_data <- merge(south_korea_ch, full_region_df, by.x = "name", by.y = "region", all.x = TRUE)
full_merged_data <- merge(full_merged_data, region_population, by.x = "name", by.y = "region", all.x = TRUE)

# Convert date column to Date class
full_merged_data$date <- as.Date(full_merged_data$date)

# Check for non-numeric values in the date column
non_numeric_dates <- full_merged_data[!is.na(as.numeric(full_merged_data$date)), ]

# Remove rows with non-numeric dates
full_merged_data <- full_merged_data[!is.na(as.numeric(full_merged_data$date)), ]


# Set min_date and max_date
min_date <- min(full_merged_data$date)
max_date <- max(full_merged_data$date)

# Create total cases
full_merged_data <- full_merged_data %>%
  group_by(date) %>%
  mutate(total_cases = sum(daily_cases))

full_merged_data <- full_merged_data %>%
  mutate(prop_cases = round(daily_cases / total_cases, 4))

# Create a column for the month
full_merged_data$month <- month(full_merged_data$date)

# Calculate the average number of cases by month and region(name)
full_merged_data <- full_merged_data %>%
  group_by(name, month) %>%
  mutate(avg_monthly_cases = round(mean(daily_cases), 0))

# Calculate the average number of cases by regions
full_merged_data$population <- as.numeric(full_merged_data$population)
full_merged_data <- full_merged_data %>%
  mutate(region_cases = round(daily_cases / population, 4))

# Remove duplicates
full_merged_data <- unique(full_merged_data)

############ color palette ###############

num1 <- seq(0, 2000, by = 250)
cv_pal_c <- colorBin("RdYlGn", domain = full_merged_data$prop_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_c_montly <- colorBin("RdYlGn", domain = full_merged_data$region_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_daily <- colorBin("RdYlGn", domain = full_merged_data$daily_cases, bins = c(0, 100, 500, 1000, 1500, 2000, Inf), reverse = TRUE)
cv_pal_avg_daily <- colorBin("RdYlGn", domain = full_merged_data$avg_monthly_cases, bins = c(0, 100, 500, 1000, 1500, 2000, Inf), reverse = TRUE)

############ centroids ###############

# Calculate centroids of the polygons
suppressWarnings({
  centroids2 <- st_centroid(full_merged_data)
  centroid_coords2 <- st_coordinates(centroids2$geometry)
  
  # Adjust the coordinates for Gyeonggi-do
  gyeonggi_coords2 <- centroid_coords2[full_merged_data$name == "Gyeonggi-do", ]
  gyeonggi_coords2[2] <- gyeonggi_coords2[2] + 0.3  # Adjust the latitude
  centroid_coords2[full_merged_data$name == "Gyeonggi-do", ] <- gyeonggi_coords2
})


# library(shinyBS)


fieldsMandatory <- c("selectedCountry", "seedData")

#hoverDrop <- "selectedCountry"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

highlightDrop <- function(menu) {
  tagList(
    menu, 
    span(class = "dropDown")
  )
}

appCSS <- ".mandatory_star {color: red;}"
appCSS <- ".invisible {display:none;}"
appCSS <- ".dropDown:hover {color:ADD8E6;background-color: #000000}"

ui <- bootstrapPage(
  tags$head(
    includeHTML("Korea viz/data/gtag.html"),
    includeCSS("Korea viz/data/custom_styles.css"),
    tags$style(HTML("
      .navbar-default .navbar-nav > li > a {
        padding-top: 15px;
        padding-bottom: 15px;
      }
      .custom-tab1 > a {
        background-color: blue;
        color: white;
      }
      .custom-tab2 > a {
        background-color: green;
        color: white;
      }
      .active > a {
        background-color: #007BFF !important;  /* Lighter shade of blue */
        color: white !important;
      }

    ")),
    tags$script("
    $(document).ready(function(){
      $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
        $('.active').removeClass('custom-tab1').removeClass('custom-tab2');
      });
    });
  ")
  ),
  add_busy_spinner(
    spin = "half-circle",       # Changing to "cube-grid" for a different fancy effect. Choose any other from the library if desired.
    color = "#112446",
    timeout = 100,
    position = "bottom-left",   # Using full-page to enable centering.
    onstart = TRUE,
    margins = c(350, 600),        # No margins to ensure proper centering.
    height = "50px",
    width = "50px"),
  navbarPage(
    theme = shinytheme("cosmo"),
    collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Covid Track Korea</a>'), 
    id="nav",
    windowTitle = "K-Covid Track",
    
    tabPanel(
      "Map",
      div(
        class="outer",
        tags$head(includeCSS("Korea viz/data/styles.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 100, left = 55, width = 300, fixed=TRUE,
                      draggable = TRUE, height = "auto", 
                      style = "background-color: #f5f5f7; border-radius: 10px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); padding: 15px; opacity: 0.95;",
                      
                      tabsetPanel(id = "tabs",
                                  tabPanel("Map 1",
                                           class = "custom-tab1",
                                           h6("Please select a date to visualize:"),
                                           
                                           sliderInput(
                                             "date_slider", "Date", 
                                             min = min_date,
                                             max = max_date,
                                             value = min_date,
                                             timeFormat="%d %b %Y"
                                           ),
                                           
                                           h6("Select the overlay group to visualize."),
                                           
                                           selectInput(
                                             "overlayGroup", "Select Overlay Group",
                                             choices = c("daily_cases", "daily_cases (proportion)")
                                           )
                                  )
                                  # ,
                                  # tabPanel("Map 2",
                                  #          class = "custom-tab2",
                                  #          h6("Please select a date range:"),
                                  #          sliderInput(
                                  #            "month_range", "Month Range",
                                  #            min = as.Date("2020-01-20"),
                                  #            max = as.Date("2023-05-27"),
                                  #            value = c(as.Date("2020-01-20"), as.Date("2023-05-27")), # Default value is the entire range
                                  #            step = 30,                       # Approximate number of days in a month
                                  #            timeFormat = "%b %Y"             # Display in "Month Year" format
                                  #          ),
                                  #          actionButton("submit", "Submit"),
                                  #          fluidRow(id = "map_container")
                                  # )
                      )
        )
      )
    ),
    
    tabPanel("Region plots",
             sidebarLayout(
               sidebarPanel(
                 span(tags$i(h6("Select the region and the outcome variable")), style="color:#045a8d"),
                
                 pickerInput("region_select", "Region:",   
                             choices <- south_korea_ch$name, 
                             selected = c("Seoul"),
                             multiple = FALSE),
                 
                 pickerInput("outcome_select", "Outcome:",   
                             choices = c("Raw new cases", "Cummulative cases", "Cummulative deaths"), 
                             selected = c("Raw new cases"),
                             multiple = FALSE),
                 
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Raw", plotlyOutput("region_plot_raw"))
                   # ,
                   # tabPanel("Smoothed", plotlyOutput("region_plot_smoothed"))
                 )
               )
             )
    ),
    
    
    tabPanel(
      title = "Model",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(appCSS),
          div(
            id = "dashboard",
            
            uiOutput("countryDropdown"),
            
            #checkboxInput(inputId = "filterLMIC", label = strong("Show LMIC only"), value = FALSE),
            
            uiOutput("clipStateCheckbox"),
            
            conditionalPanel(condition = "input.clipLev1 == '1'", uiOutput("Level1Ui")),
            
            uiOutput("aggInput"),
            
            uiOutput("modelRadio"),
            
            uiOutput("stochasticRadio"),
            
            conditionalPanel(
              id = "SEIRD_SVEIRD",
              withMathJax(),
              
              h5("Model Parameters:", style = "font-weight: bold; font-size:11.5pt"),
              
              conditionalPanel(
                id = "SVEIRD",
                withMathJax(),
                condition = "input.modelSelect == 'SVEIRD'",
                
                uiOutput("alphaInput")
              ),
              
              condition = "input.modelSelect == 'SEIRD' || input.modelSelect == 'SVEIRD'",
              
              uiOutput("betaInput"),
              
              uiOutput("gammaInput"),
              
              uiOutput("sigmaInput"),
              
              uiOutput("deltaInput"),
              
              uiOutput("lambdaInput"),
              
              radioButtons(
                inputId = "dataSelect",
                label = "Choose data source:",
                choices = c("Use provided seed data", "Upload your own seed data"),
                selected = "Use provided seed data",
                inline = FALSE
              ),
              
              uiOutput("seedUpload"),
              
              uiOutput("startDateInput"),
              
              uiOutput("timestepInput")
            ),
            
            actionButton(
              "go",
              "Run Simulation",
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            ),
            actionButton(
              "resetAll",
              "Reset Values",
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            ),
          ),
        ),
        
        mainPanel(
          tabsetPanel(
            id = 'tabSet',
            tabPanel(
              title = "Input Summary",
              icon = icon("info"),
              verbatimTextOutput("summary"),
              tableOutput("table"),
              imageOutput("outputImage"),
              imageOutput("croppedOutputImage"),
              #imageOutput("seededOutputImage"),
              #downloadButton(outputId = "downloadSummary", label = "Save Input Summary as a PDF File")
            ),
            
            tabPanel(
              title = "Mathematical Model",
              id = "modelTab",
              icon = icon("bar-chart"),
              imageOutput("modelImg")
            ),
            
            tabPanel(
              title = "Schematic Diagram",
              id = "flowchartTab",
              icon = icon("sitemap"),
              imageOutput("flowchartImg")
            ),
            
            tabPanel(
              title = "Initial Seed Data",
              dataTableOutput("tableSeed")
            ),
            
            tabPanel(
              title = "MP4 Animation",
              id = "mp4Tab",
              icon = icon("film"),
              uiOutput("outputVideo"),
              downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")
            ),
            
            tabPanel(
              title = "Output Summary",
              icon = icon("table"),
              dataTableOutput("outputSummary")
              # downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
            ),
            
            tabPanel(
              title = "Plot",
              id = "plotTab",
              icon = icon("line-chart"),
              imageOutput("infectedExposedPlot"),
              imageOutput("cumulativePlot"),
              imageOutput("fullPlot"),
              imageOutput("fracSusPlot"),
              downloadButton(outputId = "downloadPlot", label = "Save Image")
            )
          )
        ),
      )
    ),
    
    tabPanel("About this site",
             tags$div(
               tags$h4("Last update"), 
               # h6(paste0(max_date)),2023-05-27
               h6(paste0("2023-09-05")),
               # "This site is updated once daily.", tags$br(),tags$br(),
               # tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "the WHO,"),
               # tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University,"),"and",
               # tags$a(href="https://ourworldindata.org/coronavirus-data-explorer?zoomToSelection=true&time=2020-03-01..latest&country=IND~USA~GBR~CAN~DEU~FRA&region=World&casesMetric=true&interval=smoothed&perCapita=true&smoothing=7&pickerMetric=total_cases&pickerSort=desc", "Our World in Data."),
               
               "This site provides information about the spread of COVID-19 in South Korea. The data is sourced from the Korean Centers for Disease Control and Prevention (KCDC).", tags$br(),tags$br(),
               
               
               
               "The simulation model uses a mathematical model to simulate COVID-19 outcomes in South Korea based on user-defined parameters.",tags$br(),
               "The output of the model depends on model assumptions, parameter choices, and human mobility patterns.",tags$br(),
               "It is not a medical predictor, and should be used for informational and research purposes only.",tags$br(),
               "Please carefully consider the parameters you choose. Interpret and use the simulated results responsibly.",tags$br(),
               "Authors are not liable for any direct or indirect consequences of this usage.", tags$br(),tags$br(),
               
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Code")),
               "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/", "Github."),
               tags$br(),tags$br(),tags$h4(tags$b("Sources")),
               # tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
               "COVID-19 cases: ", tags$a(href="https://www.data.go.kr/en/index.do", "Public Data Portal,")," with additional information from the ",tags$a(href="https://ncov.kdca.go.kr/en/", "Central Disease Control Headquarters of South Korea."),
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Research Team")),
               "Taesung Park, PhD", tags$br(),
               "Project PI,", tags$br(),
               "Professor, Department of Statistics,", tags$br(),
               "Seoul National University,", tags$br(),
               "Gwanak_1 Gwanak-ro, Gwanak-gu", tags$br(),
               "Seoul, Korea 08826", tags$br(),tags$br(),
               
             
               "Hanbyul Song, Gyulhee Han, Catherine Apio, Jiwon Park, Zhe Liu, and Hu Xuwen", tags$br(), 
               "Graduate Student, Seoul National University, Seoul, Korea 08826",tags$br(), 
               
               
               
               tags$br(),tags$h4(tags$b("Acknowledgement")),
               "Dr. Ashok Krishnamurthy",tags$br(), 
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Contact")),
               "taesungp@gmail.com",tags$br(),
               "http://bibs.snu.ac.kr/",tags$br(),tags$br(),
               tags$img(src = "bibs.png", width = "275px", height = "75px")
               
            
             )
    )
  )
)


# setwd("C:/Users/ron18/Desktop/2022-1/bibs/mathematical model/R code-20230112T042318Z-001/R code")

server <- function(input, output, session){
  
  ############ Map tab ############
  
  output$mymap <- renderLeaflet({
    selected_date <- input$date_slider
    selected_date_range <- input$month_range
    overlayGroup <- input$overlayGroup
    filtered_data <- full_merged_data[full_merged_data$date == as.Date(selected_date), ]
    
    if(as.character(overlayGroup) == 'daily_cases'){
      cv_pal <- cv_pal_daily
    } else {
      overlayGroup <- 'prop_cases'
      cv_pal <- cv_pal_c
    }
    
    basemap <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                minZoom = 7, maxZoom = 8, dragging = TRUE)) %>%
      addTiles() %>%
      # addLayersControl(
      #   position = "bottomright",
      #   overlayGroups = c("Number of Cases (raw)", "Number of Cases (proportional)"),
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
      hideGroup(c("2019-COVID (cumulative)", "Cumulative Deaths")) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 127.5, lat = 36.2, zoom = 7) %>%
      addPolygons(
        data = filtered_data,
        fillColor = ~cv_pal(get(overlayGroup)),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(
          "<strong>", name,'</strong><br>Daily cases: ', daily_cases, '</strong><br>Daily case (proportion): ', prop_cases*100,'%', '</strong><br>Cumulative cases: ', cum_cases) %>%
          lapply(htmltools::HTML),
        labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list("font-weight" = "normal", "text-align" = "center"), textsize = "15px"),
        highlightOptions = highlightOptions(
          weight = 3,  # Increase the stroke weight of the highlighted region
          fillOpacity = 1,  # Increase the fill opacity of the highlighted region
          fillColor = "#FFCC99",  # Set the fill color of the highlighted region
          color = "#FF6600"  # Set the stroke color of the highlighted region
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = cv_pal,
        values = filtered_data[,overlayGroup],
        title = overlayGroup
      )%>%
      setMaxBounds(lng1 = 127.5 + 5,
                   lat1 = 36.2 + 3,
                   lng2 = 127.5 - 5,
                   lat2 = 36.2 - 3)
    
    basemap
    
  })
  
  # Listen to overlayGroup and displayGroup
  observe({
    # Do something with input$overlayGroup and input$displayGroup
  })
  
  # Define a reactive value to track the current maps
  current_maps <- reactiveVal()
  
  observeEvent(input$submit, {
    tryCatch({
      print("Submit button clicked")
      overlayGroup <- input$overlayGroup
      
      if(as.character(overlayGroup) == 'avg_monthly_cases'){
        cv_pal <- cv_pal_avg_daily
      } else {
        overlayGroup <- 'avg_monthly_cases'
        cv_pal <- cv_pal_avg_daily
      }
      
      
      # Get the selected date range
      start_date <- as.Date(as.yearmon(input$month_range[1]), frac = 0)
      end_date <- as.Date(as.yearmon(input$month_range[2]), frac = 1)
      
      # Get a sequence of months within the selected range
      months <- seq(from = start_date, to = end_date, by = "month")
      print(months)
      
      # Remove existing maps
      if (!is.null(current_maps())) {
        for (map_id in current_maps()) {
          removeUI(selector = paste0("#", map_id))
        }
      }
      
      # Create a container to hold the new map IDs
      new_map_ids <- character(length(months))
      
      n_columns <- 3 # Number of maps in a row
      
      # Loop over the months and generate a map for each
      for(i in seq_along(months)) {
        month_start <- months[i]
        month_end <- ifelse(i < length(months), months[i + 1] - 1, end_date)
        
        # Filter your data for the current month (modify this to fit your data)
        month_data <- full_merged_data[full_merged_data$date >= month_start & full_merged_data$date <= month_end,]
        
        print(head(month_data))
        
        # Define a unique map ID for this month
        map_id <- paste0("map", i)
        new_map_ids[i] <- map_id
        
        # Determine the row and column for the current map
        row_idx <- (i - 1) %/% n_columns
        col_idx <- (i - 1) %% n_columns
        
        # Insert a new leaflet map for the current month
        insertUI(
          selector = "#map_container",
          ui = fluidRow(
            id = map_id,
            title = format(month_start, "%B %Y"),
            leafletOutput(outputId = map_id, width = "100%", height = "400px")
          )
        )
        
        # Render the leaflet map for the current month
        output[[map_id]] <- renderLeaflet({
          # Create the leaflet map using the data for the current month (modify this to fit your visualization)
          leaflet(options = leafletOptions(zoomControl = FALSE,
                                           minZoom = 6, maxZoom = 6.1, dragging = FALSE)) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 127.5, lat = 36.2, zoom = 6) %>%
            addPolygons(
              data = month_data,
              fillColor = ~cv_pal(get(overlayGroup)),
              color = "black",
              weight = 1,
              fillOpacity = 0.7,
              label = ~paste0(
                "<strong>", name,'</strong><br>avg_monthly_cases: ', avg_monthly_cases) %>%
                lapply(htmltools::HTML),
              labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list("font-weight" = "normal", "text-align" = "center"), textsize = "15px"),
              highlightOptions = highlightOptions(
                weight = 3,  # Increase the stroke weight of the highlighted region
                fillOpacity = 1,  # Increase the fill opacity of the highlighted region
                fillColor = "#FFCC99",  # Set the fill color of the highlighted region
                color = "#FF6600"  # Set the stroke color of the highlighted region
              )
            ) %>%
            setMaxBounds(lng1 = 127.5 + 5,
                         lat1 = 36.2 + 3,
                         lng2 = 127.5 - 5,
                         lat2 = 36.2 - 3)
        })
      }
      # Update the reactive value with the new map IDs
      current_maps(new_map_ids)
      
      }, error = function(e) {
        print(e)
      })
  })
  
  
  
  ######## region tab ##########
  
  # Define a mapping between selected outcome and column name
  outcome_mapping <- c("Raw new cases" = "daily_cases",
                       "Cummulative cases" = "cum_cases",
                       "Cummulative deaths" = "cum_deaths")
  
  output$region_plot_raw <- renderPlotly({
    # Filter data based on selected region
    selected_region <- input$region_select
    selected_outcome <- input$outcome_select
    
    filtered_data <- full_merged_data[full_merged_data$name == selected_region, ]
    # Get the column name corresponding to the selected outcome from the mapping
    outcome_column <- outcome_mapping[selected_outcome]
    
    # Sort the data by date
    filtered_data <- filtered_data[order(filtered_data$date), ]
    
    # Define color palette
    point_color <- "black"
    line_color <- "black"
    
    # Create the raw plot based on the selected outcome
    plot_raw <- ggplot(filtered_data, aes(x = date, y = .data[[outcome_column]])) +
      geom_point(color = point_color, size = 1) +
      geom_line(color = line_color, size = 0.5) +
      labs(title = paste0("COVID-19 ", selected_outcome, " Over Time (Raw)"),
           x = "Date",
           y = selected_outcome) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(color = "gray"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.background = element_rect(fill = "white", color = "gray")) +
      scale_color_manual(values = c(point_color, line_color))
    
    plot_raw
  })
  
  output$region_plot_smoothed <- renderPlotly({
    # Filter data based on selected region and outcome
    selected_region <- input$region_select
    selected_outcome <- input$outcome_select
    
    filtered_data <- full_merged_data[full_merged_data$name == selected_region, ]
    
    # Get the column name corresponding to the selected outcome from the mapping
    outcome_column <- outcome_mapping[selected_outcome]
    
    # Perform smoothing (e.g., moving average) on the data
    # smoothed_data <- ...
    
    # Sort the data by date
    filtered_data <- filtered_data[order(filtered_data$date), ]
    
    # Define color palette
    point_color <- "black"
    line_color <- "black"
    
    # Create the raw plot based on the selected outcome
    plot_raw <- ggplot(filtered_data, aes(x = date, y = .data[[outcome_column]])) +
      geom_point(color = point_color, size = 1.5, shape = 13) +
      geom_line(color = line_color, size = 1) +
      labs(title = paste0("COVID-19 ", selected_outcome, " Over Time (Smoothed)"),
           x = "Date",
           y = selected_outcome) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(color = "gray"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray"),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.background = element_rect(fill = "white", color = "gray")) +
      scale_color_manual(values = c(point_color, line_color))
    
    plot_raw
  })
  
  
  ######## simulation tab ##########
  
  iv <- InputValidator$new()
  
  iv$add_rule("alpha", sv_required())
  iv$add_rule("alpha", sv_gte(0))
  
  iv$add_rule("beta", sv_required())
  iv$add_rule("beta", sv_gte(0))
  
  iv$add_rule("gamma", sv_required())
  iv$add_rule("gamma", sv_gte(0))
  
  iv$add_rule("sigma", sv_required())
  iv$add_rule("sigma", sv_gte(0))
  
  iv$add_rule("delta", sv_required())
  iv$add_rule("delta", sv_gte(0))
  
  iv$add_rule("lambda", sv_required())
  iv$add_rule("lambda", sv_gte(0))
  
  iv$add_rule("date", sv_required())
  
  iv$add_rule("timestep", sv_required())
  iv$add_rule("timestep", sv_integer())
  iv$add_rule("timestep", sv_gt(0))
  
  iv$enable()
  
  values <- reactiveValues()
  values$allow_simulation_run <- TRUE
  values$df <- data.frame(Variable = character(), Value = character()) 
  output$table <- renderTable(values$df)
  
  observeEvent(input$go, {
    if(input$clipLev1 == TRUE){
      output$croppedOutputImage <- renderImage({
        #source("R/clippingBaseRaster.R")
        source("R/clippingBaseRasterHaxby.R")
        outfile <- tempfile(fileext = '.png')
        
        png(outfile, width = 800, height = 600)
        createClippedRaster(selectedCountry = input$selectedCountry, level1Region = input$level1List, rasterAgg = input$agg)
        dev.off()
        
        list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
      }, deleteFile = TRUE)
    }
  })
  
  observeEvent(input$go, {
    output$outputImage <- renderImage({
      source("R/rasterBasePlot.R")
      outfile <- tempfile(fileext = '.png')
      
      #createBasePlot(input$selectedCountry, input$agg, FALSE) # print the susceptible plot to www/
      png(outfile, width = 800, height = 600)
      createBasePlot(input$selectedCountry, input$agg, TRUE)  # print the susceptible plot direct to UI
      dev.off()
      
      list(src = outfile, contentType = 'image/png', width = 600, height = 400, alt = "Base plot image not found")
      # The above line adjusts the dimensions of the base plot rendered in UI
    }, deleteFile = TRUE)
  })
   
  observeEvent(input$go, {
    output$modelImg <- renderImage({
      return(list(src= "www/ModelEquations.png",
                  contentType = "image/png"))
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$go, {
    output$flowchartImg <- renderImage({
      if (input$modelSelect == "SEIRD"){
        return(list(src= "www/SEIRD.png",
                    contentType = "image/png"))
      }
      else if (input$modelSelect == "SVEIRD"){
        return(list(src = "www/SVEIRD.png",
                    contentType = "image/png"))
      }
    }, deleteFile = FALSE)
  })
   
  observeEvent(input$resetAll, {
    shinyjs::reset("dashboard")
    
    shinyjs::disable(id = "go")
    values$allow_simulation_run <- FALSE
  })
  
  observeEvent(input$seedData, {
    values$allow_simulation_run <- TRUE
  })
    
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    if (isolate(values$allow_simulation_run) == TRUE){
      shinyjs::toggleState(id = "go", condition = mandatoryFilled)
    }
  })
  
  output$countryDropdown <- renderUI({
    pickerInput(inputId = 'selectedCountry',
                label = labelMandatory("Select Country"),
                # choices = population$Country,
                choices = c("South Korea"),
                selected = "South Korea", # Add this line to set South Korea as the default selected country
                options = list(
                  `actions-box` = FALSE,
                  `none-selected-text` = "Choose country",
                  `multiple-separator` = "|"
                ),
                multiple = FALSE)
  })
  
  
  output$clipStateCheckbox <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      checkboxInput(inputId = "clipLev1", label = strong("Clip State(s)/Province(s)"), value = FALSE)
    }
  })
      
  output$Level1Ui <- renderUI({
    validate(need(input$clipLev1 == TRUE, "")) # catches UI warning
    
    isoCode <- countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c")
    
    if (file.exists(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))){
      level1Options <<- readRDS(paste0("gadm/", "gadm36_", toupper(isoCode), "_1_sp.rds"))$NAME_1 
    } else {
      level1Options <<- getData("GADM", download = TRUE, level = 1, country = toupper(isoCode))$NAME_1 
    }
    
    selectizeInput(inputId = "level1List", "",
                   choices = level1Options,
                   selected = "", multiple = TRUE,
                   options = list(placeholder = "Select state(s)/province(s)"))
  })
   
  output$aggInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      sliderInput(inputId = "agg",
                  label = "Aggregation Factor",
                  min = 0, max = 100, step = 1, value = population$reco_rasterAgg[match(input$selectedCountry, population$Country)])
    }
  })
   
  output$modelRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "modelSelect",
                   label = strong("Epidemic Model"),
                   choiceValues = list("SEIRD","SVEIRD"),
                   choiceNames = list("SEIRD","SVEIRD"),
                   selected = "SVEIRD", #character(0), # 
                   inline = TRUE,
                   width = "1000px")
    }
  })
   
  output$stochasticRadio <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      radioButtons(inputId = "stochasticSelect",
                   label = strong("Model Stochasticity"),
                   choiceValues = list("Deterministic","Stochastic"),
                   choiceNames = list("Deterministic","Stochastic"),
                   selected = "Deterministic", #character(0), #
                   inline = TRUE,
                   width = "1000px")
    }
  })
  
  output$alphaInput <- renderUI({
    alphaValue <- 0.2100 # 0.00015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"alpha"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"alpha"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     alphaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"alpha"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     alphaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"alpha"])
      #   }
      # }
      
      numericInput(inputId = "alpha",
                   label = "Daily Vaccination Rate (\\( \\alpha\\)):",
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$betaInput <- renderUI({
    betaValue <- 0.055 # 0.00001
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"beta"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"beta"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     betaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"beta"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     betaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"beta"])
      #   }
      # }
      
      numericInput(inputId = "beta",
                   label = "Daily Exposure Rate (\\( \\beta\\))", 
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$gammaInput <- renderUI({
    gammaValue <- 0.009 #0.008
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"gamma"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"gamma"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     gammaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"gamma"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     gammaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"gamma"])
      #   }
      # }
      
      numericInput(inputId = "gamma",
                   label = "Daily fraction that move out of the exposed compartment to the Infected compartment  (\\( \\gamma\\))", 
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$sigmaInput <- renderUI({
    sigmaValue <- 0.065
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"sigma"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"sigma"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"sigma"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     sigmaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"sigma"])
      #   }
      # }
      
      numericInput(inputId = "sigma",
                   label = "Daily fraction that move out of the Infected compartment to the recovered compartment (\\( \\sigma \\))", 
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$deltaInput <- renderUI({
    deltaValue <- 0.0015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"delta"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"delta"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     deltaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"delta"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     deltaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"delta"])
      #   }
      # }
      
      numericInput(inputId = "delta",
                   "Daily fraction that move out of the Infected compartment to the dead compartment (\\(\\delta\\)):",
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$lambdaInput <- renderUI({
    lambdaValue <- 15
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"lambda"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"lambda"])
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"lambda"])
      #   } else if (input$selectedCountry == "Nigeria"){
      #     lambdaValue <- as.numeric(filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"lambda"])
      #   }
      # }
      
      numericInput(inputId = "lambda",
                   "Distance parameter (\\( \\lambda\\), in km):",
                   value = lambdaValue,min = 1, max = 50, step = 1)
    }
  })
  
  output$seedUpload <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      if (input$dataSelect == "Upload your own seed data") {
        fileInput(
          inputId = "seedData",
          labelMandatory("Upload initial seed data (.csv or .xls or .xlsx)"),
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xls",
            ".xlsx"
          )
        )
      } else {
        NULL
      }
    }})

  
  seedData <- reactive({
    if (input$dataSelect == "Use provided seed data") {
      # Load the provided seed data (replace 'provided_seed_data.csv' with the actual file name)
      read.csv("seeddata/KOR_initialSeedData2022-07-07 NEW.csv")
    } else {
      # If the user chooses to upload their own seed data, load the uploaded file
      req(input$seedData)
      read.csv(input$seedData$datapath)
    }
  })
  
  output$startDateInput <- renderUI({
    startDateInput <- Sys.Date() # NULL
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      # if(input$modelSelect == "SEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SEIRD")[1,"startDate"]
      #   } else if (input$selectedCountry == "Nigeria"){
      #     startDateInput <- "2020-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SEIRD")[1,"startDate"]
      #   }
      # } else if (input$modelSelect == "SVEIRD"){
      #   if (input$selectedCountry == "Czech Republic"){
      #     startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "CZE" & model == "SVEIRD")[1,"startDate"]
      #   } else if (input$selectedCountry == "Nigeria"){
      #     startDateInput <- "2021-09-01" #filter(epiparms, ISONumeric == "NGA" & model == "SVEIRD")[1,"startDate"]
      #   }
      # }
      
      dateInput('date', "Choose simulation start date:", value = startDateInput, max = Sys.Date(),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL)
    }
  })
  
  output$timestepInput <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "timestep",
                   label = "Number of Iterations (days)",
                   min = 1, max = 3650, value = 3, step = 1)
    }
  })

  output$outputVideo <- renderUI({
    tags$video(
      id = "video", 
      type = "video/mp4",
      src = "MP4/Infected_MP4.mp4",  # TODO: dynamically change which mp4 is printed
      controls = "controls"
    )
  })
  
  lineThickness <- 1.5
  
  observeEvent(input$go, {
    source("R/makePlots.R")
    output$infectedExposedPlot <- makePlot(compartments = c("E", "I"), input = input, plotTitle = paste0("Time-series plot of Exposed and Infectious compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), "Compartment Value", lineThickness = lineThickness)
    
    output$cumulativePlot <- makePlot(compartments = c("D"), input = input, plotTitle = paste0("Estimated Cumulative COVID-19 Deaths in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Cumulative Deaths", lineThickness = lineThickness)
    
    if (input$modelSelect == "SVEIRD"){
      output$fullPlot <- makePlot(compartments = c("S", "V", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
    } else {
      output$fullPlot <- makePlot(compartments = c("S", "E", "I", "R", "D"), input = input, plotTitle = paste0("Time-series plot of epidemic compartments in ", input$selectedCountry), xTitle = paste0("Day (from ", input$date, ")"), yTitle = "Compartment Value", lineThickness = lineThickness)
    }
    
  })

  observeEvent(input$go, {
    
    isCropped <- FALSE
    
    if(input$clipLev1 == TRUE)
    {
      isCropped <- TRUE
    }
    else
    {
      isCropped <- FALSE
    }
    
    print(paste0(c("isCropped", isCropped)))
    
    source("R/rasterStack.R")
    rs <- createRasterStack(input$selectedCountry, input$agg, isCropped)
    
    # ============= TAB TO SHOW SEED DATA IN TABLE ===========
    data <- reactive({               # read seed data from .csv or .xlsx
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      seedData <- input$seedData
      if(ext == 'xlsx'){
        readxl::read_excel(input$seedData$datapath)
      } else {
        read.csv(input$seedData$datapath)
      }
    })
    
    output$tableSeed <- renderDataTable({ # print initial seed data to UI
      req(input$seedData)
      if(is.null(data())){return ()}
      data()
    })
    
    output$outputSummary <- renderDataTable({ # print output summary to UI
      outputSummaryTable <- read_excel(paste0("www/MP4/", countrycode(input$selectedCountry, "country.name", "iso3c"), "_summary.xlsx"))
      outputSummaryTable
    })
    
    output$dataPlot <- renderPlot({
      buildPlot()
    })
    
    alpha <- ifelse(input$modelSelect == "SVEIRD", input$alpha, 0) # DO NOT DELETE
    beta  <- input$beta  # DO NOT DELETE
    gamma <- input$gamma # DO NOT DELETE
    sigma <- input$sigma # DO NOT DELETE
    delta <- input$delta # ifelse(input$modelSelect == "SEIR", 0, input$delta) # DO NOT DELETE
    
    source("R/rasterSimulation.R")
    
    eps <- 0.0000000000000001
    
    radius <- ifelse(input$lambda <= input$agg, 1, round(((input$lambda - input$agg)/input$agg) + eps) + 1)
    
    isDeterministic <- TRUE
    
    if(input$stochasticSelect == "Deterministic")
    {
      isDeterministic <- TRUE
    }
    else
    {
      isDeterministic <- FALSE
    }
    
    SpatialCompartmentalModel(model = input$modelSelect, startDate = input$date, selectedCountry = input$selectedCountry, directOutput = FALSE, rasterAgg = input$agg, alpha, beta, gamma, sigma, delta, radius = radius, lambda = input$lambda, timestep = input$timestep, seedFile = data(), deterministic = isDeterministic, isCropped)
    
    row1  <- data.frame(Variable = "Country", Value = input$selectedCountry)
    row2  <- data.frame(Variable = "WorldPop Raster Dimension", Value = paste0(rs$WorldPopRows, " rows x ", rs$WorldPopCols, " columns = ", rs$WorldPopCells, " grid cells"))
    row3  <- data.frame(Variable = "Aggregation Factor", Value = input$agg)
    row4  <- data.frame(Variable = "Aggregated Raster Dimension", Value = paste0(nrow(rs$rasterStack), " rows x ", ncol(rs$rasterStack), " columns = ", ncell(rs$rasterStack), " grid cells"))
    row5  <- data.frame(Variable = "Compartmental Model", Value = input$modelSelect)
    row6  <- data.frame(Variable = "Model Parameters", Value = paste("Alpha:", alpha,"Beta:", beta,"Gamma:", gamma, "Sigma:", sigma,"Delta:", delta))
    row7  <- data.frame(Variable = "Average Distance Travelled/Day (in km)", Value = input$lambda)
    row8  <- data.frame(Variable = "Radius (1 = Moore neighbourhood)", Value = radius)
    row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    row10 <- data.frame(Variable = "Number of iterations (days)", Value = input$timestep)
    
    values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10)
    
  })
  
  observe({
    updatePickerInput(session, inputId = 'selectedCountry', choices = c("South Korea"), selected = "South Korea")
  })

  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Input Summary')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Input Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Input Summary')
  })

  observe(
    hideTab(inputId = 'tabSet', target = 'Mathematical Model')
  )
  
  observeEvent(input$resetAll,{
    hideTab(inputId= 'tabSet', target = 'Mathematical Model')
  })
  
  observeEvent(input$go,{
    showTab(inputId= 'tabSet', target = 'Mathematical Model')
  })
  
  observe(
    hideTab(inputId ='tabSet', target = 'Schematic Diagram')
  )
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Schematic Diagram')
  })
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Schematic Diagram')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Initial Seed Data')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Initial Seed Data')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'MP4 Animation')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'MP4 Animation')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Output Summary')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Output Summary')
  })
  
  observeEvent(input$resetAll,{
    hideTab(inputId = 'tabSet', target = 'Plot')
  })
  
  observe(
    hideTab(inputId = 'tabSet', target = 'Plot')
  )
  
  observeEvent(input$go,{
    showTab(inputId = 'tabSet', target = 'Plot')
  })
  
}

shinyApp(ui,server)