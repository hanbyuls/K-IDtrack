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
shhh(library(DT))
shhh(library(zoo))
shhh((library(lubridate)))
shhh((library(rsconnect)))


############### Load data ###############

# setwd("C:/Users/ron18/Desktop/2022-1/bibs/covid19/kcovidtrack")
# data_korea <- read.csv("Korea viz/data/data_korea.csv")
region_population <- read_excel("Korea viz/data/region_population.xlsx")
population <- read_excel("misc/population.xlsx", 1) #?
full_region_df <- read.csv("Korea viz/data/region_data_08_31.csv")

# Read data for map
# worldcountry = geojsonio::geojson_read("Korea viz/data/input_data/50m.geojson", what = "sp")
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
full_region_df <- subset(full_region_df, !(region %in% c("Total", "Lazaretto")))

# Find indices of zero values in cum_deaths
zero_indices <- which(full_region_df$cum_deaths == 0)
# Find indices of zero values in cum_cases
zero_indices_cum_cases <- which(full_region_df$cum_cases == 0)
# Find indices of zero values in daily_cases
zero_indices_daily_case <- which(full_region_df$daily_cases == 0)


# Keep the row with the highest 'cum_cases' for each region and date
full_region_df <- full_region_df %>%
  group_by(region, date) %>%
  # Assuming 'cum_cases' can serve as a proxy for the latest data
  slice(which.max(cum_cases)) %>%
  ungroup()

full_region_df <- full_region_df %>%
  arrange(region, date) %>%
  group_by(region) %>%
  mutate(
    cum_cases = ifelse(cum_cases < lag(cum_cases, default = first(cum_cases)), lag(cum_cases), cum_cases)
  ) %>%
  ungroup()


# remove the row with "*" in the df
full_region_df <- full_region_df %>%
  mutate(
    per_100k = ifelse(grepl("\\*", per_100k), gsub("\\*", "", per_100k), per_100k),
    per_100k = as.numeric(per_100k)
  )

# First, make sure that 'cum_cases' and 'per_100k' are numeric
full_region_df <- full_region_df %>%
  mutate(
    cum_cases = as.numeric(cum_cases),
    per_100k = as.numeric(per_100k)
  )

# table(is.na(full_region_df))

# # Fix the 'cum_cases' and 'per_100k' by multiplying by 10 for those specific rows
# full_region_df <- full_region_df %>%
#   mutate(
#     cum_cases = if_else(original_row_index %in% anomalies_index$original_row_index, cum_cases * 10, cum_cases),
#     per_100k = if_else(original_row_index %in% anomalies_index$original_row_index, per_100k * 10, per_100k)
#   )


# Define a function to detect and correct anomalies
correct_cum_deaths <- function(df) {
  repeat {
    # Add a row index to the data frame for reference
    df <- df %>%
      mutate(original_row_index = row_number())
    
    # Calculate the previous day's cumulative deaths for each region
    df <- df %>%
      group_by(region) %>%
      arrange(region, date) %>%
      mutate(prev_cum_deaths = lag(cum_deaths)) %>%
      ungroup()
    
    # Detect anomalies
    anomalies <- df %>%
      filter(cum_deaths < prev_cum_deaths) %>%
      select(original_row_index, region, date, cum_deaths, prev_cum_deaths)
    
    # Break the loop if there are no more anomalies
    if (nrow(anomalies) == 0) {
      break
    }
    
    # Correct anomalies
    df <- df %>%
      mutate(
        cum_deaths = if_else(original_row_index %in% anomalies$original_row_index, prev_cum_deaths, cum_deaths)
      ) %>%
      select(-prev_cum_deaths, -original_row_index) # Clean up
  }
  
  return(df)
}

# Apply the correction function to your full_region_df
full_region_df <- correct_cum_deaths(full_region_df)



full_region_df <- full_region_df %>%
  group_by(region) %>%
  arrange(region, date) %>%
  mutate(
    prev_cum_cases = lag(cum_cases, order_by = date),
    calculated_daily_cases = cum_cases - ifelse(is.na(prev_cum_cases), 0, prev_cum_cases),
    daily_cases_correct = if_else(is.na(prev_cum_cases), daily_cases, calculated_daily_cases),
    # Flag rows where daily_cases does not match the calculated daily cases
    incorrect_daily_cases = daily_cases != calculated_daily_cases
  ) %>%
  ungroup()

incorrect_indices <- which(full_region_df$incorrect_daily_cases)

# Correct the daily_cases values
full_region_df <- full_region_df %>%
  mutate(
    daily_cases = if_else(incorrect_daily_cases, calculated_daily_cases, daily_cases)
  ) %>%
  select(-c(prev_cum_cases, calculated_daily_cases, incorrect_daily_cases, daily_cases_correct)) # Clean up helper columns

# Add daily deaths to the full_region_df
full_region_df <- full_region_df %>%
  group_by(region) %>%
  arrange(region, date) %>%
  mutate(
    prev_cum_deaths = lag(cum_deaths, order_by = date),  # Get the previous day's cumulative deaths
    daily_deaths = cum_deaths - ifelse(is.na(prev_cum_deaths), 0, prev_cum_deaths)  # Calculate the daily deaths
  ) %>%
  ungroup() %>%
  select(-prev_cum_deaths)

#remove first row of df from each region
full_region_df <- full_region_df %>%
  filter(date != "2020-01-20")

# match region names Eng/Kor
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

# table(is.na(full_merged_data))



# For plot selection
plot_name_list_single <- c("bar" = "Bar plot", 
                           "line" = "Line plot",
                           "area" = "Area plot")

plot_name_list_multi <- c("bar" = "Stacked bar plot", 
                          "line" = "Line plot",
                          "area" = "Stacked area plot")

# Total regions
region_list <- full_merged_data$name %>% unique

# Set min_date and max_date
min_date <- min(full_merged_data$date)
max_date <- max(full_merged_data$date)

# Create total cases & proportional cases
full_merged_data <- full_merged_data %>%
  group_by(date) %>%
  mutate(total_cases = sum(daily_cases))
full_merged_data <- full_merged_data %>%
  mutate(prop_cases = round(daily_cases / total_cases, 4))

# Create total deaths & proportional deaths
full_merged_data <- full_merged_data %>%
  group_by(date) %>%
  mutate(total_deaths = sum(daily_deaths))
full_merged_data <- full_merged_data %>%
  mutate(prop_deaths = round(daily_deaths / total_deaths, 4))

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

# Calculate the average number of deaths by month and region(name)
full_merged_data <- full_merged_data %>%
  group_by(name, month) %>%
  mutate(avg_monthly_deaths = round(mean(daily_deaths), 0))

# Calculate the average number of deaths by regions
full_merged_data <- full_merged_data %>%
  mutate(region_deaths = round(daily_deaths / population, 4))

# Remove duplicates
full_merged_data <- unique(full_merged_data)

############ color palette ###############

num1 <- seq(0, 2000, by = 250)
cv_pal_c <- colorBin("RdYlGn", domain = full_merged_data$prop_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_d <- colorBin("RdYlGn", domain = full_merged_data$prop_deaths, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_c_montly <- colorBin("RdYlGn", domain = full_merged_data$region_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_d_montly <- colorBin("RdYlGn", domain = full_merged_data$region_deaths, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_c_daily <- colorBin("RdYlGn", domain = full_merged_data$daily_cases, bins = c(0, 100, 500, 1000, 1500, 2000, Inf), reverse = TRUE)
cv_pal_d_daily <- colorBin("RdYlGn", domain = full_merged_data$daily_deaths, bins = c(0, 5, 10, 20, 50, 100, Inf), reverse = TRUE)
cv_pal_c_avg_daily <- colorBin("RdYlGn", domain = full_merged_data$avg_monthly_cases, bins = c(0, 100, 500, 1000, 1500, 2000, Inf), reverse = TRUE)
cv_pal_d_avg_daily <- colorBin("RdYlGn", domain = full_merged_data$avg_monthly_deaths, bins = c(0, 10, 50, 100, 150, 200, Inf), reverse = TRUE)

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


fieldsMandatory <- c("selectedCountry", "seedData")

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



############################ UI Component ############################


ui <- bootstrapPage(
  shinyjs::useShinyjs(), 
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
      .leaflet-container {
        background-color: #FAFAFA;  
      }
    ")),
    tags$script("
    $(document).ready(function(){
      $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {
        $('.active').removeClass('custom-tab1').removeClass('custom-tab2');
      });
    });
    $(document).on('shiny:connected', function(event) {
      var w = window.innerWidth;
      var h = window.innerHeight;
      Shiny.setInputValue('screen_width', w);
      Shiny.setInputValue('screen_height', h);
    });
  ")
  ),
  navbarPage(
    theme = shinytheme("cosmo"),
    collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">K-Covid Track</a>'), 
    id="nav",
    windowTitle = "K-Covid Track",
    
    tabPanel(
      "Map",
      div(
        class="outer",
        tags$head(includeCSS("Korea viz/data/styles.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 100, left = 55, width = 350, fixed=TRUE,
                      draggable = TRUE, height = 500, 
                      style = "background-color: #f5f5f7; border-radius: 10px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); padding: 15px; opacity: 0.95;",
                      
                      tabsetPanel(id = "tabs",
                                  tabPanel("Map",
                                           class = "custom-tab1",
                                           h6("Please select a date to visualize:", style = "font-size: 18px;"),  # Increase font-size to 18px
                                           
                                           sliderInput(
                                             "date_slider", "Date", 
                                             min = min_date,
                                             max = max_date,
                                             value = min_date,
                                             timeFormat = "%d %b %Y",
                                             width = "100%"  # Set width to 100%
                                           ),
                                           
                                           h6("Select the overlay group to visualize.", style = "font-size: 16px;"),  # Increase font-size to 18px
                                           
                                           selectInput(
                                             "overlayGroup", "Select Overlay Group",
                                             choices = c("daily_cases", "daily_cases (proportion)", "daily_deaths", "daily_deaths (proportion)"),
                                             width = "100%"  # Set width to 100%
                                           ),
                                           
                                           h6("daily_cases (proportion): daily cases by region / total daily cases", style = "font-size: 14px;"),  # Increase font-size to 16px
                                           h6("daily_deaths (proportion): daily deaths by region / total daily deaths", style = "font-size: 14px;")
                                  )
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
                             multiple = TRUE),
                 
                 pickerInput("outcome_select", "Outcome:",   
                             choices = c("Daily cases", "Cummulative cases", "Daily deaths", "Cummulative deaths"), 
                             selected = c("Daily cases"),
                             multiple = FALSE),
                 
                 #Input for p_type
                 selectInput("p_type_input", "Select Plot Type:", 
                             choices = c("area", "line", "bar"), 
                             selected = "area"),
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
      title = "Simulation",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          shinyjs::inlineCSS(appCSS),
          div(
            id = "dashboard",

            textInput("selectedCountry", "Selected Country", value = "South Korea"),

            hidden(
              checkboxInput(inputId = "filterLMIC", label = strong("Show LMIC only"), value = FALSE)
            ),
            uiOutput("clipStateCheckbox"),
            
            conditionalPanel(condition = "input.clipLev1 == '1'", uiOutput("Level1Ui")),
            
            uiOutput("aggInput"),
            
            uiOutput("modelRadio"),
            
            uiOutput("stochasticRadio"),
            
            conditionalPanel(
              id = "SEIRD_SVEIRD",
              withMathJax(),
              
              h5("Model Parameters:", style = "font-weight: bold; font-size:13pt"),
              
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
              
              downloadButton("downloadSeedData", "Download Sample Seed Data"),
              
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
               h6(paste0("2023-11-06")),
               
               "This site provides information about the spread of COVID-19 in South Korea. The data is sourced from the Korean Centers for Disease Control and Prevention (KCDC).", tags$br(),
               "Please be advised that, following the reclassification of COVID-19 as a Class 4 infectious disease by the KCDC, the regular updates to the provincial incidence data have been discontinued as of September 1, 2023.", tags$br(),tags$br(),
               
               
               "The simulation model uses a mathematical model to simulate COVID-19 outcomes in South Korea based on user-defined parameters.",tags$br(),
               "The output of the model depends on model assumptions, parameter choices, and human mobility patterns.",tags$br(),
               "It is not a medical predictor, and should be used for informational and research purposes only.",tags$br(),
               "Please carefully consider the parameters you choose. Interpret and use the simulated results responsibly.",tags$br(),
               "Authors are not liable for any direct or indirect consequences of this usage.", tags$br(),tags$br(),
               
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Code")),
               "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/ron1891/K-CovidTrack", "Github."),
               tags$br(),tags$br(),tags$h4(tags$b("Sources")),
               "COVID-19 cases: ", tags$a(href="https://www.data.go.kr/en/index.do", "Public Data Portal,")," with additional information from the ",tags$a(href="https://ncov.kdca.go.kr/en/", "Central Disease Control Headquarters of South Korea."),
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Research Team")),
               "Taesung Park, PhD", tags$br(),
               "Project PI,", tags$br(),
               "Professor, Department of Statistics,", tags$br(),
               "Seoul National University,", tags$br(),
               "Gwanak_1 Gwanak-ro, Gwanak-gu", tags$br(),
               "Seoul, Korea 08826", tags$br(),tags$br(),
               
               
               "Hanbyul Song, Gyulhee Han, Taewan Goo, Catherine Apio, Jiwon Park, and Zhe Liu", tags$br(), 
               "Graduate Student, Seoul National University, Seoul, Korea 08826",tags$br(), 
               
               
               
               tags$br(),tags$h4(tags$b("Acknowledgement")),
               "Dr. Ashok Krishnamurthy",tags$br(), 
               
               
               tags$br(),tags$br(),tags$h4(tags$b("Contact")),
               "tspark@stats.snu.ac.kr",tags$br(),
               "byul1891@gmail.com",tags$br(),
               "http://bibs.snu.ac.kr/",tags$br(),tags$br(),
               tags$img(src = "bibs.png", width = "275px", height = "75px")
               
               
             )
    )
  )
)





############################ Server Component ############################


# setwd("C:/Users/ron18/Desktop/2022-1/bibs/mathematical model/R code-20230112T042318Z-001/R code")

server <- function(input, output, session){
  
  ############ Map tab ############
  
  observe({
    
    width <- as.numeric(input$screen_width)
    height <- as.numeric(input$screen_height)
    
    if(is.null(width) || length(width) == 0) return()
    
    if(width > 1200) {
      focus_lat <- 36.2
      focus_lng <- 127.5
      zoom_level <- 10
    } else {
      focus_lat <- 36.2
      focus_lng <- 127.5
      zoom_level <- 5
    }
    
    output$mymap <- renderLeaflet({
      selected_date <- input$date_slider
      selected_date_range <- input$month_range
      overlayGroup <- input$overlayGroup
      filtered_data <- full_merged_data[full_merged_data$date == as.Date(selected_date), ]
      
      
      if(as.character(overlayGroup) == 'daily_cases'){
        overlayGroup <- 'daily_cases'
        cv_pal <- cv_pal_c_daily
        display_data <- "cases"
      } else if (as.character(overlayGroup) == 'daily_cases (proportion)') {
        overlayGroup <- 'prop_cases'
        cv_pal <- cv_pal_c
        display_data <- "cases"
        filtered_data$prop_cases[is.na(filtered_data$prop_cases)] <- 0
      } else if (as.character(overlayGroup) == 'daily_deaths') {
        overlayGroup <- 'daily_deaths'
        cv_pal <- cv_pal_d_daily
        display_data <- "deaths"
      } else if (as.character(overlayGroup) == 'daily_deaths (proportion)') {
        overlayGroup <- 'prop_deaths'
        cv_pal <- cv_pal_d  
        display_data <- "deaths"
        filtered_data$prop_deaths[is.na(filtered_data$prop_deaths)] <- 0
      }
      
      # Define a reactive expression for labels
      labelContent <- reactive({
        if (display_data == "cases") {
          return(filtered_data %>%
                   mutate(label_text = paste0("<strong>", name, "</strong><br>Daily cases: ", daily_cases,
                                              "<br>Daily case (proportion): ", round(prop_cases * 100, 2), '%',
                                              "<br>Cumulative cases: ", cum_cases)) %>%
                   .$label_text %>%
                   lapply(htmltools::HTML))
        } else if (display_data == "deaths") {
          return(filtered_data %>%
                   mutate(label_text = paste0("<strong>", name, "</strong><br>Daily deaths: ", daily_deaths,
                                              "<br>Daily death (proportion): ", round(prop_deaths * 100, 2), '%',
                                              "<br>Cumulative deaths: ", cum_deaths)) %>%
                   .$label_text %>%
                   lapply(htmltools::HTML))
        }
      })
      
      basemap <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                  minZoom = 7, maxZoom = 8, dragging = TRUE)) %>%

        hideGroup(c("2019-COVID (cumulative)", "Cumulative Deaths")) %>%
        setView(lng = 127.5, lat = 36.2, zoom = 7) %>%
        addPolygons(
          data = filtered_data,
          fillColor = ~cv_pal(get(overlayGroup)),
          color = "black",
          weight = 1,
          fillOpacity = 0.7,
          label = ~labelContent(),
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
        cv_pal <- cv_pal_c_avg_daily
      } else {
        overlayGroup <- 'avg_monthly_cases'
        cv_pal <- cv_pal_c_avg_daily
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
            # addTiles() %>%
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
  outcome_mapping <- c("Daily cases" = "daily_cases",
                       "Cummulative cases" = "cum_cases",
                       "Daily deaths" = "daily_deaths",
                       "Cummulative deaths" = "cum_deaths")
  
  output$region_plot_raw <- renderPlotly({
    # Filter data based on selected region
    selected_region <- input$region_select
    selected_outcome <- input$outcome_select
    p_type <- input$p_type_input
    
    min_date <- min(full_merged_data$date)
    max_date <- max(full_merged_data$date)
    
    region_order <- full_merged_data %>% filter(date == max(full_merged_data$date)) %>% 
      arrange(-cum_cases) %>% .[["name"]]
    
    # Get the column name corresponding to the selected outcome from the mapping
    outcome_column <- outcome_mapping[selected_outcome]
    
    filtered_data_m <- full_merged_data %>% #filter(date >= as.Date("2022-01-01")) %>% 
      mutate(name = factor(name, levels = region_order)) %>% 
      filter(name %in% selected_region) %>% 
      mutate(name = droplevels(name)) %>% 
      rename(y = outcome_column %>% as.character()) %>% ungroup() %>% 
      dplyr::select(name, date, y)
    
    if(length(selected_region) == 1){
      
      if(p_type == "area"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name)) +
          geom_area(alpha = 0.7) +
          geom_line(size = 0.5, col = "tomato") 
      }else if(p_type == "line"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name, col = name)) +
          geom_line(size = 0.5)
      }else if(p_type == "bar"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name)) +
          geom_col(width = 1, alpha = 0.8, col = "tomato")
      }else{
        break
      }
      
      # add titles
      plot_raw_m <- plot_raw_m +
        labs(title = paste0("Spread of COVID-19 by Region"),
             subtitle = paste0(plot_name_list_single[[p_type]], ", ", selected_outcome, " (", selected_region, ")"),
             x = "Date",
             y = selected_outcome, fill = "Region", col = "tomato") +
        guides(fill = guide_legend(title = "Region"), 
               color = guide_legend(title = "Region"))
      
      
    }else if(length(selected_region) > 1){
      if(p_type == "area"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name, col = name)) +
          geom_area(alpha = 0.7)
      }else if(p_type == "line"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name, col = name)) +
          geom_line(size = 0.5, alpha = 0.7)
      }else if(p_type == "bar"){
        plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name, col = name)) +
          geom_col(width = 1, position = "stack", alpha = 0.8)
        
      }else{
        break
      }
      
      if(length(selected_region) == 2){
        # add titles
        plot_raw_m <- plot_raw_m +
          labs(title = paste0("Spread of COVID-19 by Region"),
               subtitle = paste0(plot_name_list_multi[[p_type]], ", ", selected_outcome, " (", paste(selected_region, collapse = ", "), ")"),
               x = "Date",
               y = selected_outcome, fill = "Region", col = "Region")
      }else{
        plot_raw_m <- plot_raw_m +
          labs(title = paste0("Spread of COVID-19 by Region"),
               subtitle = paste0(plot_name_list_multi[[p_type]], ", ", selected_outcome, " (", length(selected_region), " regions)"),
               x = "Date",
               y = selected_outcome, fill = "Region", col = "Region")
      }
    }
    
    plot_raw_m <- plot_raw_m +
      theme_bw() +
      theme(plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black", vjust=2),
            plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="maroon", vjust=2),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(color = "gray"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "gray"),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.background = element_rect(fill = "white", color = "gray"))

    plotly_obj <- ggplotly(plot_raw_m, height = 450) %>% 
      layout(
        xaxis = list(
          rangeslider = list(
            type = "date",
            rangemode = "auto",
            thickness = 0.12,
            start = min_date,
            end = max_date
          )
        )
      )
    
    plotly_obj
 
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
  
  observe({
    shinyjs::hide("countryDropdown")
    shinyjs::hide("clipStateCheckbox")
    shinyjs::hide("Level1Ui")
    input$selectedCountry == "South Korea"
    input$clipLev1 == FALSE
  })
  
  observeEvent(input$go, {
    req(input$selectedCountry, input$clipLev1, input$level1List, input$agg)
    
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
    req(input$selectedCountry, input$agg)
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
    req(input$modelSelect)
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
  
  
  shinyjs::hide("selectedCountry")
  
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
      
      numericInput(inputId = "alpha",
                   label = "Daily Vaccination Rate (\\( \\alpha\\)):",
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$betaInput <- renderUI({
    betaValue <- 0.055 # 0.00001
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "beta",
                   label = "Daily Exposure Rate (\\( \\beta\\))", 
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$gammaInput <- renderUI({
    gammaValue <- 0.009 #0.008
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "gamma",
                   label = "Daily fraction that move out of the exposed compartment to the Infected compartment  (\\( \\gamma\\))", 
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$sigmaInput <- renderUI({
    sigmaValue <- 0.065
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "sigma",
                   label = "Daily fraction that move out of the Infected compartment to the recovered compartment (\\( \\sigma \\))", 
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$deltaInput <- renderUI({
    deltaValue <- 0.0015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "delta",
                   "Daily fraction that move out of the Infected compartment to the dead compartment (\\(\\delta\\)):",
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$lambdaInput <- renderUI({
    lambdaValue <- 15
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "lambda",
                   "Distance parameter (\\( \\lambda\\), in km):",
                   value = lambdaValue,min = 1, max = 50, step = 1)
    }
  })
  
  
  output$downloadSeedData <- downloadHandler(
    filename = function() {
      "seeddata/KOR_initialSeedData2022-07-07.csv"  # The name of the file that will be downloaded
    },
    content = function(file) {
      file.copy("seeddata/KOR_initialSeedData2022-07-07.csv", file)  # Copy the file to the download location
    }
  )
  
  output$seedUpload <- renderUI({
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      # if (input$dataSelect == "Upload seed data") {
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
      # } else {
      #   NULL
      # }
    }})

  
  output$startDateInput <- renderUI({
    startDateInput <- Sys.Date() # NULL
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
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