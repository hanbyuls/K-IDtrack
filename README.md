## Interactive web-based dashboard for analyzing geographical and temporal spread of COVID-19 in South Korea

This github page provides the code and data for the K-CovidTrack dashboard. It contains information about the spread of COVID-19 in South Korea. 

The data is sourced from the Korean Centers for Disease Control and Prevention (KCDC).


## K-CovidTrack dashboard

Follow [this](https://covidtrack.shinyapps.io/kcovidtrack/) link for the interactive Shiny app. 


## Code explanation

Key elements of the analysis code are as follows:
- *input_data.R* – an R script which extracts daily COVID-19 cases via API from the Korean Centers for Disease Control and Prevention (KCDC). The data is is saved into the main directory.
- *app.R* - an R script responsible for displaying the Shiny app. It encompasses various graphing functions and the necessary ui (user interface) and server logic to showcase the Shiny app.


## Research Team

Taesung Park, PhD
Project PI,
Professor, Department of Statistics,
Seoul National University,
Gwanak_1 Gwanak-ro, Gwanak-gu
Seoul, Korea 08826

Hanbyul Song, Gyulhee Han, Catherine Apio, Jiwon Park and Zhe Liu
Graduate Student, Seoul National University, Seoul, Korea 08826


## Acknowledgement

Dr. Ashok Krishnamurthy


## Contact

taesungp@gmail.com

http://bibs.snu.ac.kr/
