## Interactive web-based dashboard for analyzing geographical and temporal spread of COVID-19 in South Korea

This github page provides the code and data for the K-InfectDisTrack: COVID dashboard. It contains information about the spread of COVID-19 in South Korea. 

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

Ashok Krishnamurthy, PhD
Associate Professor, Department of Mathematics and Computing, 
Mount Royal University, 
4825 Mount Royal Gate SW Calgary, 
Alberta, Canada

Hanbyul Song, Gyulhee Han, Taewan Gu, Jiwon Park and Zhe Liu
Graduate Student, Seoul National University, Seoul, Korea 08826


## Acknowledgement



## Contact

tspark@stats.snu.ac.kr

byul1891@gmail.com

http://bibs.snu.ac.kr/
