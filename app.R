# rm(list = ls()); gc()

#install.packages("C:/test/k-track-covid/maptools_1.1-8.tar.gz")
#library(maptools)
options(conflicts.policy = list(warn = FALSE))
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(av))
shhh(library(bslib))
shhh(library(cptcity))
shhh(library(countrycode))
shhh(library(plyr))
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
# shhh(library(dplyr))
shhh(library(leaflet))
shhh(library(leaflet.extras))
shhh(library(htmltools))
shhh(library(shinyBS))
shhh(library(plotly))
shhh(library(shinydashboard))
shhh(library(DT))
shhh(library(zoo))
shhh(library(shinybusy))
shhh((library(lubridate)))
shhh((library(rsconnect)))
shhh((library(HatchedPolygons)))
shhh((library(shinyRatings)))
shhh((library(ShinyRating)))
shhh((library(bslib)))

####################################################################################

# For plot selection
plot_name_list_single <- c("bar" = "Bar plot", 
                           "line" = "Line plot",
                           "area" = "Area plot")

plot_name_list_multi <- c("bar" = "Stacked bar plot", 
                          "line" = "Line plot",
                          "area" = "Stacked area plot")



population <- read_excel("misc/population.xlsx", 1) 

# socio-demographic data
load("Korea viz/data/full_merged_sociodemo_df.rdata")

# covid-19 cases and deaths
load("Korea viz/data/full_merged_data.rdata")

# source year of socio-demographic data
load("Korea viz/data/full_merged_sociodemo_year.rdata")


full_merged_sociodemo_df <- full_merged_sociodemo_df %>% as.data.frame() %>% select(-geometry)
full_merged_sociodemo_year <- full_merged_sociodemo_year %>% as.data.frame() %>% select(-geometry)

gc()

south_korea <- sf::st_read("Korea viz/data/south-korea-with-regions_1516.geojson") #region lon,lat
region_match <- read.csv("Korea viz/data/region_match.csv") # english/korean name
south_korea_ch <- south_korea
south_korea_ch$name <- region_match$eng[match(south_korea_ch$name, region_match$kor)]


# Set min_date and max_date
min_date <- min(full_merged_data$date)
max_date <- max(full_merged_data$date)

region_names <- full_merged_data$name %>% unique %>% sort

############ color palette ###############

num1 <- seq(0, 2000, by = 250)
cv_pal_c <- colorBin("RdYlGn", domain = full_merged_data$prop_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_d <- colorBin("RdYlGn", domain = full_merged_data$prop_deaths, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_c_montly <- colorBin("RdYlGn", domain = full_merged_data$region_cases, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_d_montly <- colorBin("RdYlGn", domain = full_merged_data$region_deaths, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.4, 1), reverse = TRUE)
cv_pal_c_daily <- colorBin("RdYlGn", domain = full_merged_data$daily_cases, bins = c(0, 100, 500, 1000, 1500, 2000, Inf), reverse = TRUE)
cv_pal_d_daily <- colorBin("RdYlGn", domain = full_merged_data$daily_deaths, bins = c(0, 5, 10, 20, 50, 100, Inf), reverse = TRUE)
cv_pal_c_daily_100k <- colorBin("RdYlGn", domain = full_merged_data$per_100k_cases, bins = c(0, 5, 10, 50, 100, 500, Inf), reverse = TRUE)
cv_pal_d_daily_100k <- colorBin("RdYlGn", domain = full_merged_data$per_100k_deaths, bins = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, Inf), reverse = TRUE)


names(full_merged_data)
names(full_merged_sociodemo_df)


cv_pal_so_male<- colorBin("RdYlGn", domain = full_merged_sociodemo_df$male, bins = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 400000, 500000, Inf), reverse = TRUE)
cv_pal_so_female <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$female, bins = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 400000, 500000, Inf), reverse = TRUE)
cv_pal_so_seniors_per_1k <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$seniors_per_1k, bins = c(0, 5, 10, 15, 20, 25, 30), reverse = TRUE)
cv_pal_so_foreigners_per_1k <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$foreigners_per_1k, bins = c(0, 5, 10, 15, 20, 25, 30, 35), reverse = TRUE)
cv_pal_so_smoking_rate <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$smoking_rate, bins = c(0, 5, 10, 15, 20, 25), reverse = TRUE)
cv_pal_so_bed <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$beds_per_1k, bins = c(0, 5, 10, 15, 20, 25, 30), reverse = TRUE)
cv_pal_so_doctors_per_1k <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$doctors_per_1k, bins = 0:6, reverse = TRUE)
cv_pal_so_income <- colorBin("RdYlGn", domain = full_merged_sociodemo_df$income, bins = c(0, 5000, 10000, 15000, 20000, 25000, 30000), reverse = TRUE)


gc()

full_merged_data <- full_merged_data %>% as.data.frame() %>% select(-geometry)
gc()

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

gc()

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
      .jumbotron {
        position: relative; /* 상대적 위치 설정 */
      }
      
      #floating-buttons {
        position: absolute; /* 절대적 위치 설정 */
        bottom: 20px; /* jumbotron 하단에서 20px 위 */
        right: 20px; /* jumbotron 우측에서 20px 왼쪽 */
      }
      
      .floating-btn {
        display: inline-block; /* 버튼을 가로로 배열 */
        margin-right: 10px; /* 오른쪽 버튼과의 간격 */
        padding: 5px 15px; /* 버튼 내부 여백 */
        background-color: white; /* 배경색 */
        color: #286090; /* 글자색 */
        border-radius: 8px; /* 둥근 모서리 */
        text-decoration: none; /* 밑줄 없음 */
        border: 1px solid #546cc2; /* 테두리 색상 */
        transition: background-color 0.3s, border-color 0.3s; /* 색상 변화 애니메이션 */
        text-align: center; /* 텍스트 가운데 정렬 */
      }
      .floating-btn:hover, .floating-btn:active, .floating-btn:focus {
        background-color: #286090; /* 호버 시 배경색 */
        color: white; /* 호버 시 글자색 */
        border-color: #286090; /* 호버 시 테두리색 */
        text-decoration: none !important; /* 강제로 밑줄 없애기 */
        outline: none !important; /* 클릭 시 나타나는 외곽선 제거 */
      }
      .navbar { /* Bootstrap 3의 기본 navbar */
        background-color: #27293E !important; /* 새 배경색 */
        border-color: #27293E !important; /* 테두리 색상 일치 */
      }
      .navbar-default .navbar-nav > li > a {
        padding-top: 15px;
        padding-bottom: 15px;
        
      }
      .navbar-brand img { margin-left: 15px; margin-top: -15px; margin-bottom: -15px; height: 100%; }
      .navbar { min-height: 50px; }
      .navbar .navbar-brand {
      display: flex;
      align-items: center;
      justify-content: center;
      height: 100%;
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
    tags$script(HTML("
      $(document).ready(function() {
        // Make the navbar brand (title) clickable, redirecting to the base URL or performing a custom action
        $('.navbar-brand').click(function() {
          // Example: Redirect to base URL, or you can modify this to show/hide content programmatically
          window.location.href = 'https://k-track-covid.shinyapps.io/k-track-covid/';
        });
      });
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
    windowTitle = "K-Track-Covid",
    
    # HTML 함수를 사용하여 메인 페이지 링크 대신 이미지로 타이틀을 설정
    # 이미지를 클릭하면 지정된 웹페이지로 이동
    title = HTML('<div class="navbar-brand" style="padding: 0;"><img src="k-track-covid.png" style="width: 300px; height: 51px;"></div>'),
    #title = HTML('<a class="navbar-brand" href="https://github.com/hanbyuls/K-Track-Covid" target="_blank"><img src="k-track-covid.png" height="50"></a>'),
    id="nav",
    collapsible = TRUE,
    # Define the Main Page tab with ID 'main'.
    tabPanel("Main Page", id="main",
             div(class = "container",
                 tags$div(class = "jumbotron", 
                          tags$h2("Welcome to", style = "color: #337ab7; font-weight: bold; font-size: 30px;"), # Adjusted font size for the 'Welcome' message
                          tags$h1("K-Track-Covid dashboard", style = "font-weight: bold; font-size: 44px;"), # Adjusted font size for the dashboard title
                          tags$br(),
                          tags$p("The K-Track-Covid is an interactive web-based dashboard developed using the R Shiny framework, to offer users an intuitive dashboard for analyzing the geographical and temporal spread of COVID-19 in South Korea.", style="font-size: 24px;"), # Adjusted font size for paragraph text
                          tags$p("The dashboard is designed to assist researchers, policymakers, and the public in understanding the spread and impact of COVID-19, thereby facilitating informed decision-making. ", style="font-size: 24px;"),
                          tags$br(),
                          
                          tags$h3("Featured Sections:", style="font-weight: bold;font-size: 28px;"), # Adjusted font size for the 'Featured Sections' header
                          tags$ul(
                            tags$li("Map Tab: Explore an interactive map detailing COVID-19 cases and trends across different regions.", style="font-size: 24px;"),
                            tags$li("Regional Trend Tab: Delve into time-series visualizations of COVID-19 metrics for individual regions.", style="font-size: 24px;"),
                            tags$li("Simulation Tab: Utilize advanced mathematical models to simulate various COVID-19 scenarios.", style="font-size: 24px;")
                          ),
                          tags$br(),
                          div(id="floating-buttons",
                              a(href="http://statgen.snu.ac.kr/software/user-manual/KTrack-Covid.pdf", target="_blank", class="floating-btn", "Manual"),
                              a(href="https://forms.gle/7w3sLqLFmM8NdfXX8", target="_blank", class="floating-btn", "Feedback")
                              
                          )
                 )
             )
    ),
    tabPanel(
      "Map",
      div(
        class="outer",
        tags$head(includeCSS("Korea viz/data/styles.css")),
        leafletOutput("mymap", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 100, left = 55, width = 450, fixed=TRUE,
                      draggable = TRUE, height = 950, 
                      style = "background-color: #f5f5f7; border-radius: 10px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); padding: 15px; opacity: 0.95;",
                      
                      
                      
                      tabsetPanel(id = "tabs",
                                  tabPanel("Map",
                                           class = "custom-tab1",
                                           
                                           div(
                                             style = "display: inline-block; vertical-align: middle;",
                                             h6("Please select a date to visualize", style = "display: inline-block; font-size: 18px; margin-right: 5px;"),
                                             circleButton("show_map", icon = icon("question"), size = "xs", 
                                                          style = "display: inline-block; vertical-align: middle;")
                                             # actionButton("my_button", "Click Me", 
                                             #              style = "display: inline-block; vertical-align: middle;")
                                           ),
                                           #circleButton("show_map", icon = icon("question"), size = "snm"),
                                           
                                           
                                           #h6("Please select a date to visualize:", style = "font-size: 18px;"),  # Increase font-size to 18px
                                           
                                           sliderInput(
                                             "date_slider", "Date", 
                                             min = min_date,
                                             max = max_date,
                                             value = min_date,
                                             timeFormat = "%d %b %Y",
                                             width = "100%"  # Set width to 100%
                                           ),
                                           
                                           ############ 추가 ############
                                           
                                           radioButtons("dataChoice", "Choose Data to Visualize:",
                                                        choices = list("COVID-19 cases and deaths" = "1", "Socio-demographic variables" = "2", "Both" = "both"),
                                                        selected = "1"),
                                           h6("Select the overlay group to visualize.", style = "font-size: 16px;"),  # Increase font-size to 18px
                                           conditionalPanel(
                                             condition = "input.dataChoice == '1' || input.dataChoice == 'both'",
                                             uiOutput("varSelectUI1"),
                                             checkboxInput("showHighRisk1", "Show high risk zone for COVID-19 data", FALSE, width = "90%"),
                                             uiOutput("thresholdSlider1")
                                           ),
                                           conditionalPanel(
                                             condition = "input.dataChoice == '2' || input.dataChoice == 'both'",
                                             uiOutput("varSelectUI2"),
                                             checkboxInput("showHighRisk2", "Show high risk for Socio-demographic data", FALSE, width = "90%"),
                                             uiOutput("highval_risk"),
                                             uiOutput("thresholdSlider2")
                                           ),
                                           conditionalPanel(
                                             condition = "input.dataChoice == 'both'",
                                             selectInput("mainData", "Select Main Data:",
                                                         choices = c("COVID-19 cases and deaths" = "1", "Socio-demographic variables" = "2"),
                                                         selected = "1", width = "90%")
                                           ),
                                           
                                  )
                                  
                      )
        )
      )
    ),
    
    tabPanel("Regional Trend",
             sidebarLayout(
               sidebarPanel(
                 span(tags$i(h6("Select the region and the outcome variable")), style="color:#045a8d"),
                 
                 pickerInput("region_select", "Region:",   
                             choices <- region_names, 
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
                 )
               )
             )
    ),
    
    
    tabPanel(
      title = "Simulation",
      actionButton("show_simulation", "Details"),
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
              
              
              radioButtons("dataSelect", "Seed data:",
                           choices = list("Use provided seed data" = "default",
                                          "Upload your own seed data" = "upload"),
                           selected = "default",
                           inline = FALSE),
              
              
              
              uiOutput("seedUpload"),
              
              uiOutput("fileInputUI"),
              
              uiOutput("startDateInput"),
              
              uiOutput("timestepInput")
            ),
            
            
            actionButton("go", "Run Simulation",
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", disabled = TRUE),

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
              imageOutput("croppedOutputImage")
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
              uiOutput("outputVideo")#,
              #downloadButton(outputId = "downloadMP4", label = "Save MP4 Animation")
            ),
            
            tabPanel(
              title = "Output Summary",
              icon = icon("table"),
              dataTableOutput("outputSummary"),
              downloadButton(outputId = "downloadOutputSummary", label = "Save Output Summary")
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
               h6(paste0("2024-04-04")),
              
               "This site provides information about the spread of COVID-19 in South Korea. The data is sourced from the Korean Centers for Disease Control and Prevention (KCDC).", tags$br(),
               "Please be advised that, following the reclassification of COVID-19 as a Class 4 infectious disease by the KCDC, the regular updates to the provincial incidence data have been discontinued as of September 1, 2023.", tags$br(),tags$br(),
               
               
               "The simulation model uses a mathematical model to simulate COVID-19 outcomes in South Korea based on user-defined parameters.",tags$br(),
               "The output of the model depends on model assumptions, parameter choices, and human mobility patterns.",tags$br(),
               "It is not a medical predictor, and should be used for informational and research purposes only.",tags$br(),
               "Please carefully consider the parameters you choose. Interpret and use the simulated results responsibly.",tags$br(),
               "Authors are not liable for any direct or indirect consequences of this usage.", tags$br(),tags$br(),
               

               tags$br(),tags$br(),tags$h4(tags$b("Code")),
               "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/hanbyuls/K-Track-Covid", "Github."),
               tags$br(),tags$br(),tags$h4(tags$b("Sources")),
               "COVID-19 cases: ", tags$a(href="https://www.data.go.kr/en/index.do", "Public Data Portal,")," with additional information from the ",tags$a(href="https://www.kdca.go.kr/index.es?sid=a3", "Central Disease Control Headquarters of South Korea."),
               
               
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
               tags$img(src = "bibs.png", width = "116px", height = "50px")
               
               
             )
    )
  )
)





############################ Server Component ############################

# resp_only, demo_only, both 
plot_options <- "resp_only"
main_plot_option <- "no"

covid_var_names <- list("daily_cases" = "daily_cases", "daily_cases (proportion)"= "prop_cases", "daily_cases (per 100K)" = "per_100k_cases", 
                        "daily_deaths"= "daily_deaths", "daily_deaths (proportion)" = "prop_deaths", "daily_deaths (per 100K)" = "per_100k_deaths")

# <15, 15–24, 25–49, 50–64, 65–79, and 80+ years
socio_var_names <- list("Population - males" = "male", "Population - females"= "female", 
                        # "Population - age < 25" = "age_0_24",
                        # "Population - age 25~64" = "age_25_64",
                        # "Population - age > 64" = "age_65+",
                        "Population - seniors (per 1K)" = "seniors_per_1k",
                        "Population - foreigners (per 1K)" = "foreigners_per_1k",
                        "Comorbidity - smoking rate" = "smoking_rate",
                        "Health - total beds (per 1K)" = "beds_per_1k",
                        "Health - doctors (per 1K)" = "doctors_per_1k",
                        "Economic - income (1,000 ￦)" = "income")

high_default <-  list("male" = "high", 
                      "female" = "high",
                      "seniors_per_1k" = "high",
                      "foreigners_per_1k" = "high",
                      "smoking_rate" = "high",
                      "beds_per_1k" = "low",
                      "doctors_per_1k" = "low",
                      "income" = "low")

server <- function(input, output, session){
  
  ############ Map tab ############
  
  # 변수 선택 UI for COVID-19 data
  output$varSelectUI1 <- renderUI({
    if (input$dataChoice == "1" || input$dataChoice == "both") {
      selectInput("overlayGroup", "Variable for COVID-19 data", choices = covid_var_names, selected = "daily_cases", width = "90%")
    }
  })

  # 변수 선택 UI for Socio-demographic data
  output$varSelectUI2 <- renderUI({
    if (input$dataChoice == "2" || input$dataChoice == "both") {
      selectInput("overlayGroup2", "Variable for Socio-demographic data", choices = socio_var_names, selected = "beds_per_1k", width = "90%")
    }
  })

  # 임계값 슬라이더 UI for COVID-19 data
  output$thresholdSlider1 <- renderUI({
    if (input$showHighRisk1 && (input$dataChoice == "1" || input$dataChoice == "both")) {
      
      # case, deaths, cases/100k, deaths/100k
      if(input$overlayGroup == "daily_cases"){
        max_val <- 10000
      }else if(input$overlayGroup == "daily_deaths"){
        max_val <- 200
      }
      else if(input$overlayGroup == "per_100k_cases"){
        max_val <- 1000
      }else if(input$overlayGroup == "per_100k_deaths"){
        max_val <- 0.5
      }else{
        max_val <- max(full_merged_data[[input$overlayGroup]], na.rm = TRUE)
      }
      
      sliderInput("threshold1", "Threshold for COVID-19 data", min = min(full_merged_data[[input$overlayGroup]], na.rm = TRUE),
                  max = max_val, value = mean(full_merged_data[[input$overlayGroup]], na.rm = TRUE), width = "100%" )
      
    }
  })

  # 임계값 슬라이더 UI for Socio-demographic data
  output$thresholdSlider2 <- renderUI({
    #print(input$overlayGroup2)
    if (input$showHighRisk2 && (input$dataChoice == "2" || input$dataChoice == "both")) {
      full_merged_sociodemo_df[[input$overlayGroup2]] <- as.numeric(full_merged_sociodemo_df[[input$overlayGroup2]])
      sliderInput("threshold2", "Threshold for Socio-demographic data", min = min(full_merged_sociodemo_df[[input$overlayGroup2]], na.rm = TRUE),
                  max = max(full_merged_sociodemo_df[[input$overlayGroup2]], na.rm = TRUE), value = mean(full_merged_sociodemo_df[[input$overlayGroup2]], na.rm = TRUE), width = "100%" )
    }
  })

  output$highval_risk <- renderUI({
    # print("here")
    # print(is.na(input$showHighRisk2))
    if(length(input$showHighRisk2) > 0){
      if (input$showHighRisk2 && (input$dataChoice == "2" || input$dataChoice == "both")) {
        selectInput("highval_risk", label = NULL, choices = list("High value means higher risk"="high", "High value means lower risk"="low"), 
                    selected = high_default[[input$overlayGroup2]], width = "90%")
      }
    }
    
  })

  
  ################################################################################
  
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

      
      
      #selected_date <- "2022-01-01"
      df_1 <- full_merged_data[full_merged_data$date == as.Date(selected_date), ] # resp
      df_1 <- merge(south_korea_ch, df_1, by.x = "name", by.y = "name", all.x = TRUE)
      
      df_2 <- full_merged_sociodemo_df[full_merged_sociodemo_df$date == as.Date(selected_date), ] # sodemo
      df_2 <- merge(south_korea_ch, df_2, by.x = "name", by.y = "name", all.x = TRUE)
      
      # print(object.size(df_1), 
      #       quote = FALSE, units = "Mb", standard = "auto", digits = 1L)
      
      df_2_source <- full_merged_sociodemo_year[full_merged_sociodemo_year$date == as.Date(selected_date), ] # sodemo
      
      
      
      #print(input$mainData)
      if(input$dataChoice =='1' | (input$dataChoice =='both' & input$mainData == '1')){ # + resp이 main
        
        
        #overlayGroup <- "daily_cases"
        overlayGroup <- input$overlayGroup
        filtered_data <- df_1
        #filtered_data2 <- [full_merged_data$date == as.Date(selected_date), ]
        
        if(as.character(overlayGroup) == 'daily_cases'){
          overlayGroup <- 'daily_cases'
          cv_pal <- cv_pal_c_daily
          display_data <- "cases"
        } else if (as.character(overlayGroup) == 'prop_cases') {
          overlayGroup <- 'prop_cases'
          cv_pal <- cv_pal_c
          display_data <- "cases"
          filtered_data$prop_cases[is.na(filtered_data$prop_cases)] <- 0
        } else if (as.character(overlayGroup) == 'per_100k_cases') {
          overlayGroup <- 'per_100k_cases'
          cv_pal <- cv_pal_c_daily_100k
          display_data <- "cases"
        }else if (as.character(overlayGroup) == 'daily_deaths') {
          overlayGroup <- 'daily_deaths'
          cv_pal <- cv_pal_d_daily
          display_data <- "deaths"
        } else if (as.character(overlayGroup) == 'prop_deaths') {
          overlayGroup <- 'prop_deaths'
          cv_pal <- cv_pal_d  
          display_data <- "deaths"
          filtered_data$prop_deaths[is.na(filtered_data$prop_deaths)] <- 0
        }else if (as.character(overlayGroup) == 'per_100k_deaths') {
          overlayGroup <- 'per_100k_deaths'
          cv_pal <- cv_pal_d_daily_100k  
          display_data <- "deaths"
          filtered_data$per_100k_deaths[is.na(filtered_data$per_100k_deaths)] <- 0
        }
        
        
      }else if(input$dataChoice =='2' | (input$dataChoice =='both' & input$mainData == '2')){
        
        overlayGroup <- input$overlayGroup2
        #overlayGroup <- "beds_per_1k"
        df_2[[overlayGroup]] <- as.numeric(df_2[[overlayGroup]])
        filtered_data <- df_2
        if(as.character(overlayGroup) == 'beds_per_1k'){
          cv_pal <- cv_pal_so_bed
          display_data <- "beds"
        }else if(as.character(overlayGroup) == 'male'){
          cv_pal <- cv_pal_so_male
          display_data <- "male"
        }else if(as.character(overlayGroup) == 'female'){
          cv_pal <- cv_pal_so_female
          display_data <- "female"
        }else if(as.character(overlayGroup) == 'seniors_per_1k'){
          cv_pal <- cv_pal_so_seniors_per_1k
          display_data <- "seniors_per_1k"
        }else if(as.character(overlayGroup) == 'foreigners_per_1k'){
          cv_pal <- cv_pal_so_foreigners_per_1k
          display_data <- "foreigners_per_1k"
        }else if(as.character(overlayGroup) == 'smoking_rate'){
          cv_pal <- cv_pal_so_smoking_rate
          display_data <- "smoking_rate"
        }else if(as.character(overlayGroup) == 'doctors_per_1k'){
          cv_pal <- cv_pal_so_doctors_per_1k
          display_data <- "doctors_per_1k"
        }else if(as.character(overlayGroup) == 'income'){
          cv_pal <- cv_pal_so_income
          display_data <- "income"
        }
        
        
      }else{
        print("error")
      }
      
      if(input$showHighRisk1){
        risk_1_idx <- which(df_1[[input$overlayGroup]] > input$threshold1)

        #risk_1_idx <- which(df_1[["daily_cases"]] > 1000)
        if(length(risk_1_idx)>0){
          df_1_risk <- df_1[risk_1_idx,]
        }
      }
      if(input$showHighRisk2){
        df_2[[input$overlayGroup2]] <- as.numeric(df_2[[input$overlayGroup2]])
        # print("here2")
        # print(is.na(input$highval_risk))
        if(input$highval_risk == "high"){
          risk_2_idx <- which(df_2[[input$overlayGroup2]] > input$threshold2)
        }else if(input$highval_risk == "low"){
          risk_2_idx <- which(df_2[[input$overlayGroup2]] < input$threshold2)
        }
        if(length(risk_2_idx)>0){
          df_2_risk <- df_2[risk_2_idx,]
        }
      }
      
      # Define a reactive expression for labels
      labelContent <- reactive({
        if (display_data == "cases") {
          return(filtered_data %>%
                   mutate(label_text = paste0("<strong>", name, "</strong><br>Daily cases: ", daily_cases,
                                              "<br>Daily case (proportion): ", round(prop_cases * 100, 2), '%',
                                              "<br>Daily case (per 100K): ",  per_100k_cases,
                                              "<br>Cumulative cases: ", cum_cases)) %>%
                   .$label_text %>%
                   lapply(htmltools::HTML))
        } else if (display_data == "deaths") {
          return(filtered_data %>%
                   mutate(label_text = paste0("<strong>", name, "</strong><br>Daily deaths: ", daily_deaths,
                                              "<br>Daily death (proportion): ", round(prop_deaths * 100, 2), '%',
                                              "<br>Daily death (per 100K): ",  per_100k_deaths,
                                              "<br>Cumulative deaths: ", cum_deaths)) %>%
                   .$label_text %>%
                   lapply(htmltools::HTML))
        }else{
          filtered_data$value <- filtered_data[[overlayGroup]]
          filtered_data$source_year <- df_2_source[[overlayGroup]]
          return(filtered_data %>%
                   mutate(label_text = paste0("<strong>", name, "</strong><br>",display_data,": ", value,
                                              "<br>*Source year: ", source_year)) %>%
                   .$label_text %>%
                   lapply(htmltools::HTML))
        }
      })
      
      basemap <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                  minZoom = 7, maxZoom = 8, dragging = TRUE)) %>%

        hideGroup(c("2019-COVID (cumulative)", "Cumulative Deaths")) %>%
        # addProviderTiles(providers$CartoDB.Positron) %>%
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
      
      if(input$dataChoice =='1' | input$dataChoice =='both'){

        if(input$showHighRisk1){
          if(length(risk_1_idx)>0){
            
            df_1_hatched <- hatched.SpatialPolygons(df_1_risk, density = c(8), angle = c(135))
            
            
            basemap <- basemap %>%
              addPolylines(
                data = df_1_hatched,
                color = c("black"),
                weight = 1,
                opacity = 1
              )
          }
        }
      }
      
      
      if(input$dataChoice =='2' | input$dataChoice =='both'){

        if(input$showHighRisk2){
          if(length(risk_2_idx)>0){
            
            df_2_hatched <- hatched.SpatialPolygons(df_2_risk, density = c(8), angle = c(90))
            
            
            basemap <- basemap %>%
              addPolylines(
                data = df_2_hatched,
                color = c("black"),
                weight = 1,
                opacity = 1
              )
          }
        }
      }
      
      basemap
      
    })
  })

  
  # Listen to overlayGroup and displayGroup
  observe({
    # Do something with input$overlayGroup and input$displayGroup
  })
  
  # Define a reactive value to track the current maps
  current_maps <- reactiveVal()
  
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
        # plot_raw_m <- ggplot(filtered_data_m, aes(x = date, y = y, fill = name)) +
        #   geom_line(size = 0.5, alpha = 0.7, col = "tomato")
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
    
    # plot_raw_m
    
  })
  
  ######## simulation tab ##########
  
  observeEvent(input$show_map, {
    showModal(modalDialog(
      title = "Details", 
      
      tags$p("Proportional data:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("e.g. daily_cases (proportion) & daily_deaths (proportion).", style="font-size: 14px;"),
      tags$p("daily_cases (proportion) = daily cases by region / total daily cases", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("Divide by total population data:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("e.g. daily_cases (per 100K) & total beds (per 1K).", style="font-size: 14px;"),
      tags$p("daily_cases (per 100K) = daily cases by region / total daily cases", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("High risk zone for COVID-19 data:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("higher value means high risk.", style="font-size: 14px;"),
      tags$p("high-risk zones are represented by diagonal stripes on the map.", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("High risk zone for Socio-demographic data:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("user can select high risk group (e.g. higher beds means low risk).", style="font-size: 14px;"),
      tags$p("high-risk zones are represented by vertical stripes on the map.", style="font-size: 14px;"),
      tags$br()
    ))
  })
  
  observeEvent(input$show_simulation, {
    showModal(modalDialog(
      title = "User Manual: Simulation Tab", 
      tags$p("Aggregation Factor:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("Aggregation factor reduces the resolution of the result. Higher aggregation factor value means lower resolution", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("Epidemic Model:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("SEIRD = Susceptible, Exposed, Infected, Recovered, Dead.", style="font-size: 14px;"),
      tags$p("SVEIRD = Susceptible, Vaccinated, Exposed, Infected, Recovered, Dead.", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("Model Stochasticity:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("The deterministic model relies on fixed, precise inputs without accounting for randomness, while stochastic model incorporate randomness and uncertainty in the prediction.", style="font-size: 14px;"),
      tags$br(),
      
      tags$p("Model Parameters:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("\u03B1 = Vaccination Rate", style="font-size: 14px;"),  # Alpha
      tags$p("\u03B2 = Daily Exposure Rate", style="font-size: 14px;"),  # Beta
      tags$p("\u03B3 = Daily fraction that move out of the exposed compartment to the Infected compartment", style="font-size: 14px;"),  # Gamma
      tags$p("\u03C3 = Daily fraction that move out of the Infected compartment to the recovered compartment", style="font-size: 14px;"),  # Sigma
      tags$p("\u03B4 = Daily fraction that move out of the Infected compartment to the dead compartment", style="font-size: 14px;"),  # Delta
      tags$p("\u03BB = Distance parameter", style="font-size: 14px;"),  # Lambda
      tags$br(),
      
      tags$p("Seed data:", style="font-weight: bold;font-size: 16px;"), 
      tags$p("User can choose between using the default seed data, or uploading the seed data before running the simulation.", style="font-size: 14px;"),
      tags$br(),
      
    ))
  })
  
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
  #values$allow_simulation_run <- TRUE
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
    
    #values$allow_simulation_run <- FALSE
  })
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && (length(which(input[[x]] == ""))==0)
             },
             logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
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
                   label = HTML("Vaccination rate (&alpha;)"), # Using HTML entity for alpha
                   value = alphaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$betaInput <- renderUI({
    betaValue <- 0.055 # 0.00001
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "beta",
                   label = HTML("Daily Exposure rate (&beta;)"), 
                   value = betaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$gammaInput <- renderUI({
    gammaValue <- 0.009 #0.008
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      numericInput(inputId = "gamma",
                   label = HTML("Exposed -> Infected (&gamma;)"), 
                   value = gammaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$sigmaInput <- renderUI({
    sigmaValue <- 0.065
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "sigma",
                   label = HTML("Infected -> Recovered (&sigma;)"), 
                   value = sigmaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$deltaInput <- renderUI({
    deltaValue <- 0.0015
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "delta",
                   HTML("Infected -> Dead (&delta;)"),
                   value = deltaValue, min = 0, max = 1, step = 0.00001)
    }
  })
  
  output$lambdaInput <- renderUI({
    lambdaValue <- 15
    
    validate(need(!is.null(input$selectedCountry), "")) # catches UI warning
    
    if (!is.null(input$selectedCountry) && input$selectedCountry != ""){
      
      numericInput(inputId = "lambda",
                   HTML("Distance parameter (&lambda; in km)"),
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
  
  output$downloadManual <- downloadHandler(
    filename = function() {
      "user_manual.pdf"  # The name of the file that will be downloaded
    },
    content = function(file) {
      file.copy("www/user_manual.pdf", file)  # Copy the file to the download location
    }
  )

  
  output$downloadOutputSummary <- downloadHandler(
    filename = function() {
      "KOR_summary.xlsx"  # The name of the file that will be downloaded
    },
    content = function(file) {
      file.copy("www/MP4/KOR_summary.xlsx", file)  # Copy the file to the download location
    }
  )

  
  # Dynamically render the file input UI based on dataSelect
  output$fileInputUI <- renderUI({
    if(input$dataSelect == "upload") {
      fileInput("seedData", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    }
  })
  
  # Observe changes in dataSelect and seedData to manage go button state
  observe({
    # Enable or disable the go button based on dataSelect and whether a file has been uploaded
    if(input$dataSelect == "default" || (input$dataSelect == "upload" && !is.null(input$seedData))) {
      shinyjs::enable("go")
    } else {
      shinyjs::disable("go")
    }
  })
  
  
  output$seedUpload <- eventReactive(input$go, {
    if(input$dataSelect == "default") {
      read.csv("seeddata/KOR_initialSeedData2022-07-07.csv")
    } else if(input$dataSelect == "upload" && !is.null(input$seedData)) {
      read.csv(input$seedData$datapath)
    } else {
      data.frame()  # Returns an empty data frame as a fallback
    }
  })
  
  useDefaultData <- reactive({
    input$dataSelect == "default" || (input$dataSelect == "upload" && is.null(input$seedData))
  })

  
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
      controls = NA
      # controls = "controls"
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
    # Reactive expression to read the data based on the file type or use the default dataset
    data <- reactive({
      req(input$go)  # Ensure this reacts to the 'go' button click
      
      # Load default data if 'Use default seed data' is selected
      if(input$dataSelect == "default") {
        return(read.csv("seeddata/KOR_initialSeedData2022-07-07.csv"))
      }
      
      # If the 'Upload your own seed data' option is selected and a file is uploaded
      req(input$seedData)
      ext <- tools::file_ext(input$seedData$datapath)
      if(ext == 'xlsx'){
        return(readxl::read_excel(input$seedData$datapath))
      } else {  # Assume .csv for all other cases
        return(read.csv(input$seedData$datapath))
      }
    })
    
    output$tableSeed <- renderDataTable({ # print initial seed data to UI
      if(input$dataSelect == "default") {
        data()
      }else{
        req(input$seedData)
        if(is.null(data())){return ()}
        data()
      }
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
    print(input$seedData$name)
    if(input$dataSelect == "default"){
      row9  <- data.frame(Variable = "Uploaded Seed Data", Value = "seeddata_KOR_initialSeedData_default.csv")
    }else{
      row9  <- data.frame(Variable = "Uploaded Seed Data", Value = input$seedData$name)
    }
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
