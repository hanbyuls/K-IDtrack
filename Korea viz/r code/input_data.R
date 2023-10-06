rm(list = ls()); gc()

library("xml2")
library(tidyverse)
library(lubridate)
library(dplyr)
library(httr)
# Load required packages
library(tidyverse)

getwd()
setwd("C:/Users/ron18/Desktop/2022-1/bibs/covid19/spatialEpisim/Korea viz/data")

# Read data from CSV file
data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter data for South Korea
data_korea <- data %>%
  filter(location == "South Korea")

# Select relevant columns
data_korea <- data_korea %>%
  select(date, new_cases, new_deaths, icu_patients) %>%
  rename(cases = new_cases, deaths = new_deaths, icu = icu_patients)

# Add region column
data_korea$region <- "South Korea"

# remove rows with na
data_korea <- na.omit(data_korea)


# Write data to CSV file
write.csv(data_korea, "covid_data.csv", row.names = FALSE)



# Read population data from CSV file
pop_data <- read.csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")

# Filter population data for South Korea
pop_data_korea <- pop_data %>%
  filter(Country_Code == "KOR")

pop_data_korea$Country <- "South Korea"
pop_data_korea <- pop_data_korea[, c("Country", "Country_Code", "Year_2016")]
colnames(pop_data_korea)[3] <- "Population"



# Join COVID-19 data with population data
data_korea <- left_join(data, pop_data_korea, by = c("region" = "Country"))

# Calculate cases per million population
data_korea$cases_per_million <- data_korea$cases / data_korea$Population * 1e6

# Write data to CSV file
write.csv(data_korea, "data_korea.csv", row.names = FALSE)




#path <- "~/covid_19/Korea_Prep/"
path <- "C:/Users/ron18/Desktop/2022-1/bibs/mathematical model/R code-20230112T042318Z-001/R code/Spatial Epidemic Models/spatialEpisim/api/"
ref_path <- paste0(path, "Ref/")
data_path <- paste0(path, "Data/")
code_path <- paste0(path, "Code/")
save_path <- paste0(path,"Plot/")


############################################# XML to Data Frame #################################################

# Service Key: Open API에서 데이터를 신청하면 발급해주는 Key
service_key <- "lsJNzdPyZQOdjGDJLyKRmVMwPbMh9M%2FqwC%2FaMhwDOZBKY5W6jfHkcvM2UHbd8cDinyQ%2B1SbtmREjmUVUvjlP2Q%3D%3D"
start_date <- "20230101"   #20230101
end_date <- "20230531"
end_date <- today()-2

#end_date <- "20201120"
# end_date를 today로 설정하더라도 업데이트가 덜 된 경우 

# Regional from Open API
# 여러 Option을 조합해 데이터를 불러오는 형태. 아래 예시에서는 start date와 end date만 변수로 지정해줌.
xml_path <- paste0("http://apis.data.go.kr/1352000/ODMS_COVID_04/callCovid04Api?serviceKey=",service_key,"&std_day=",end_date,"&")
t <- GET(paste0('http://apis.data.go.kr/1352000/ODMS_COVID_04/callCovid04Api?serviceKey=', service_key, '&pageNo=1&numOfRows=500&apiType=xml&std_day=2021-12-16'))
t <- GET(paste0('http://apis.data.go.kr/1352000/ODMS_COVID_04/callCovid04Api?serviceKey=', service_key, '&std_day=', end_date,'&'))


raw_xml <- read_xml(xml_path)
xml_node <- xml_children(raw_xml)
# xml_node_2: xml 파일의 body에 포함되는 element
xml_node_2 <- xml_children(xml_node)
# xml_items: xml_node_2의 세 번째 노드인 items이 우리가 원하는 확진자 정보를 나타내므로 해당 노드의 element를 call
xml_items <- xml_node_2[[3]] %>% xml_children()

# 각 xml_items가 한 날짜, 한 지역의 확진자 수를 포함하고 있으므로 해당 items
df <- lapply(seq_along(xml_items),
             function(i){
               temp_row <- xml_find_all(xml_items[i], "./*")
               tibble(
                 idx = i,
                 key = temp_row %>% xml_name(),
                 value = temp_row %>% xml_text()
               ) %>% return()
             }
) %>% bind_rows() %>%
  spread(key, value) %>%
  select(xml_items %>% xml_children() %>% xml_name() %>% unique())


# df <- read.csv("C:/Users/ron18/Desktop/2022-1/bibs/mathematical model/R code-20230112T042318Z-001/R code/Korea viz/data/region_df_07_20.csv")

head(df)
region_df <- df %>%
  select(stdDay, gubunEn, defCnt, deathCnt, localOccCnt, qurRate ) %>%
  rename(date = stdDay, region=gubunEn, cum_cases=defCnt, cum_deaths=deathCnt, daily_cases=localOccCnt, per_100k=qurRate)
head(region_df)

# write.csv(region_df, "C:/Users/ron18/Desktop/2022-1/bibs/covid19/spatialEpisim/Korea viz/data/region_data_05_31.csv", row.names = FALSE)

# write.csv(region_df, "C:/Users/ron18/Desktop/2022-1/bibs/covid19/spatialEpisim/Korea viz/data/region_data.csv", row.names = FALSE)



