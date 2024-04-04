# rm(list = ls()); gc()

# Load necessary libraries
library(readxl)
library(tidyr)
library(dplyr)


setwd("C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack")

######################################################
# Original data processing code
######################################################

region_population <- read_excel("Korea viz/data/region_population.xlsx")
population <- read_excel("misc/population.xlsx", 1) #?
full_region_df <- read.csv("Korea viz/data/region_data_08_31.csv")
south_korea <- sf::st_read("Korea viz/data/south-korea-with-regions_1516.geojson") #region lon,lat
region_match <- read.csv("Korea viz/data/region_match.csv") # english/korean name


# region_age <- read_excel("Korea viz/data/region_age.xlsx")
# region_bed <- read_excel("Korea viz/data/region_hospital2.xlsx")


# # Display the structure of the dataframe
# str(region_bed_raw)
# # Show the first few rows of the dataframe
# head(region_bed_raw)


######################################################

# data processing

######################################################

# # Region bed df
# colnames(region_bed) <- region_bed[1,]
# region_bed <- region_bed[-c(1,2),]
# 
# # Region age df
# colnames(region_age) <- region_age[3,]
# 
# # change 강원특별 자치도 to 강원도
# region_age$행정기관[region_age$행정기관 == '강원특별자치도'] <- '강원도'
# region_bed$행정구역별[region_bed$행정구역별 == '강원특별자치도'] <- '강원도'
# 
# 
# region_age <- region_age[-c(1, 2, 3, 4), ]
# region_age <- region_age[-1]
# region_age_m <- region_age[,c(1,17:27)]
# region_age_f <- region_age[,c(1,30:40)]
# region_age_total <- region_age[,1:14]
# 
# 
# # Add english region name
# region_age_total$region <- region_match$eng[match(region_age_total$행정기관, region_match$kor)]
# region_bed$region <- region_match$eng[match(region_bed$행정구역별, region_match$kor)]
# region_age_total <- select(region_age_total, -행정기관)
# region_bed <- select(region_bed, -행정구역별)
# 
# 
# region_age_total <- region_age_total[,-c(1,2)]
# 
# # Replace the old names with the new names
# names(region_bed)[names(region_bed) == "인구 천명당 의료기관병상수(A÷B×1,000) (개)"] <- "beds_per_1k"
# names(region_bed)[names(region_bed) == "총병상수(A) (개)"] <- "total_beds"
# names(region_bed)[names(region_bed) == "주민등록인구(B) (명)"] <- "Registered Population (B)"


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
  dplyr::select(-c(prev_cum_cases, calculated_daily_cases, incorrect_daily_cases, daily_cases_correct)) # Use dplyr::select() to specify the dplyr package

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

# Add per_100k_cases and per_100k_deaths
full_merged_data$per_100k_cases <- full_merged_data$daily_cases / full_merged_data$population * 100000
full_merged_data$per_100k_deaths <- full_merged_data$daily_deaths / full_merged_data$population * 100000

# # Merge COVID-19 data with South Korea age population and hospital information
# full_merged_data <- merge(full_merged_data, region_age_total, by.x = "name", by.y = "region", all.x = TRUE)
# full_merged_data <- merge(full_merged_data, region_bed, by.x = "name", by.y = "region", all.x = TRUE)
# 
# full_merged_data$per_100k_cases <- full_merged_data$daily_cases / full_merged_data$population * 100000
# full_merged_data$per_100k_deaths <- full_merged_data$daily_deaths / full_merged_data$population * 100000


######################################################
# Original data processing code
######################################################

region_age_raw <- read_excel("Korea viz/data/socio_demographic_data/region_sex_age.xlsx")
region_sex_raw <- read_excel("Korea viz/data/socio_demographic_data/region_sex_age.xlsx")
region_smoking_raw <- read_excel("Korea viz/data/socio_demographic_data/region_smoking.xlsx")
region_foreigner_raw <- read_excel("Korea viz/data/socio_demographic_data/region_foreigner.xlsx")
region_doctor_raw <- read_excel("Korea viz/data/socio_demographic_data/region_doctor.xlsx")
region_senior_raw <- read_excel("Korea viz/data/socio_demographic_data/region_senior.xlsx")
region_income_raw <- read_excel("Korea viz/data/socio_demographic_data/region_income.xlsx")
region_bed_raw  <- read_excel("Korea viz/data/socio_demographic_data/region_bed.xlsx")

# Ensure the 'date' column in full_merged_data is of Date type and extract the year
full_merged_data$date <- as.Date(full_merged_data$date)
full_merged_data$year <- as.numeric(format(full_merged_data$date, "%Y"))


########## sex_raw processing
{
  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_sex_raw)[4:ncol(region_sex_raw)])))
  
  colnames(region_sex_raw) <- as.character(region_sex_raw[1, ])
  region_sex_raw <- region_sex_raw[-c(1:7), ]
  
  # Assuming region_sex_raw$`특성별(2)` is the column with NAs
  region_sex_raw$`특성별(2)` <- na.locf(region_sex_raw$`특성별(2)`)
  region_sex_raw <- region_sex_raw[region_sex_raw$`성별(1)` == "남자" | region_sex_raw$`성별(1)` == "여자", ]
  
  # Columns to remove
  columns_to_remove <- c("특성별(1)", "0세", "1 - 4세", "5 - 9세", "10 - 14세", "15 - 19세", "20 - 24세",
                         "25 - 29세", "30 - 34세", "35 - 39세", "40 - 44세", "45 - 49세", "50 - 54세",
                         "55 - 59세", "60 - 64세", "65 - 69세", "70 - 74세", "75 - 79세", "80 - 84세", "85세 이상" )
  # Remove specified columns
  region_sex_raw <- region_sex_raw[, !(names(region_sex_raw) %in% columns_to_remove)]
  
  region_sex_2020 <- region_sex_raw[1:3]
  region_sex_2021 <- region_sex_raw[c(1,2,4)]
  region_sex_2022 <- region_sex_raw[c(1,2,5)]
  
  region_sex_2020$date <- years[1]
  region_sex_2021$date <- years[2]
  region_sex_2022$date <- years[3]
  
  region_sex <- rbind(region_sex_2020, region_sex_2021, region_sex_2022)
  
  names(region_sex) <- c("region", "0", "1-4", "5-9", "10-14", "15-19", "20-24",
                         "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                         "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85", "year")
  
  names(region_sex) <- c("region", "sex", "population", "year")
  
  region_sex <- region_sex %>%
    select(
      year, region, sex, population
    )
  
  region_sex$region[region_sex$region == '강원'] <- '강원도'
  region_sex$region[region_sex$region == '서울'] <- '서울특별시'
  region_sex$region[region_sex$region == '부산'] <- '부산광역시'
  region_sex$region[region_sex$region == '대구'] <- '대구광역시'
  region_sex$region[region_sex$region == '인천'] <- '인천광역시'
  region_sex$region[region_sex$region == '광주'] <- '광주광역시'
  region_sex$region[region_sex$region == '대전'] <- '대전광역시'
  region_sex$region[region_sex$region == '울산'] <- '울산광역시'
  region_sex$region[region_sex$region == '세종'] <- '세종특별자치시'
  region_sex$region[region_sex$region == '경기'] <- '경기도'
  region_sex$region[region_sex$region == '충북'] <- '충청북도'
  region_sex$region[region_sex$region == '충남'] <- '충청남도'
  region_sex$region[region_sex$region == '전북'] <- '전라북도'
  region_sex$region[region_sex$region == '전남'] <- '전라남도'
  region_sex$region[region_sex$region == '경북'] <- '경상북도'
  region_sex$region[region_sex$region == '경남'] <- '경상남도'
  region_sex$region[region_sex$region == '제주'] <- '제주특별자치도'
  
  region_sex$population <- as.numeric(region_sex$population)
  male_df <- region_sex %>% 
    filter(sex == "남자") %>%
    select(year, region, population) %>%
    rename(male = population)
  female_df <- region_sex %>% 
    filter(sex == "여자") %>%
    select(year, region, population) %>%
    rename(female = population)
  region_sex <- full_join(male_df, female_df, by = c("year", "region"))
}

most_recent_year_in_years <- max(years)

# Make sure the 'year' column in region_sex is numeric
region_sex$year <- as.numeric(region_sex$year)
# Identify the most recent year in region_sex
most_recent_year <- max(region_sex$year)
# Adjust region_sex to include entries for future years, if needed
if(max(full_merged_data$year) > most_recent_year) {
  # Identify the years that need to be added
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  # Duplicate the most recent year data for each missing year
  recent_year_data <- region_sex %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  # Combine with the original region_sex data
  region_sex <- bind_rows(region_sex, future_data)
}

region_dates <- expand.grid(region = unique(region_sex$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
expanded_region_sex <- merge(region_dates, region_sex, by = c("region", "year"), all.x = TRUE)
expanded_region_sex <- expanded_region_sex %>% arrange(region, date)

# # Identify the most recent year in your 'years' vector
most_recent_year_in_years <- max(years)

# This column will simply copy the year for now, but it's set up for future adjustments if needed
expanded_region_sex$source_year <- expanded_region_sex$year
expanded_region_sex$adjusted_for_future <- expanded_region_sex$year > most_recent_year_in_years
expanded_region_sex$source_year <- ifelse(expanded_region_sex$year > most_recent_year_in_years, most_recent_year, expanded_region_sex$year)
expanded_region_sex <- expanded_region_sex %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column
expanded_region_sex <- expanded_region_sex %>%
  select(
    date, region, source_year, male, female
  )


########## age_raw processing
{
  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_age_raw)[4:ncol(region_age_raw)])))
  
  colnames(region_age_raw) <- as.character(region_age_raw[1, ])
  region_age_raw <- region_age_raw[-c(1:7), ]
  
  region_age_raw <- region_age_raw[region_age_raw$`성별(1)` == "계", ]
  # Columns to remove
  columns_to_remove <- c("특성별(1)", "성별(1)", "합계")
  # Remove specified columns
  region_age_raw <- region_age_raw[, !(names(region_age_raw) %in% columns_to_remove)]
  
  region_age_2020 <- region_age_raw[1:20]
  region_age_2021 <- region_age_raw[c(1,21:39)]
  region_age_2022 <- region_age_raw[c(1,40:58)]
  
  region_age_2020$date <- years[1]
  region_age_2021$date <- years[2]
  region_age_2022$date <- years[3]
  
  region_age <- rbind(region_age_2020, region_age_2021, region_age_2022)
  
  names(region_age) <- c("region", "0", "1-4", "5-9", "10-14", "15-19", "20-24",
                             "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                             "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85", "year")
  
  region_age <- region_age %>%
    select(
      year, region, "0", "1-4", "5-9", "10-14", "15-19", "20-24",
      "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
      "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85"
    )
  
  region_age$region[region_age$region == '강원'] <- '강원도'
  region_age$region[region_age$region == '서울'] <- '서울특별시'
  region_age$region[region_age$region == '부산'] <- '부산광역시'
  region_age$region[region_age$region == '대구'] <- '대구광역시'
  region_age$region[region_age$region == '인천'] <- '인천광역시'
  region_age$region[region_age$region == '광주'] <- '광주광역시'
  region_age$region[region_age$region == '대전'] <- '대전광역시'
  region_age$region[region_age$region == '울산'] <- '울산광역시'
  region_age$region[region_age$region == '세종'] <- '세종특별자치시'
  region_age$region[region_age$region == '경기'] <- '경기도'
  region_age$region[region_age$region == '충북'] <- '충청북도'
  region_age$region[region_age$region == '충남'] <- '충청남도'
  region_age$region[region_age$region == '전북'] <- '전라북도'
  region_age$region[region_age$region == '전남'] <- '전라남도'
  region_age$region[region_age$region == '경북'] <- '경상북도'
  region_age$region[region_age$region == '경남'] <- '경상남도'
  region_age$region[region_age$region == '제주'] <- '제주특별자치도'

}

most_recent_year_in_years <- max(years)

# Ensure the 'date' column in full_merged_data is of Date type and extract the year
full_merged_data$date <- as.Date(full_merged_data$date)
full_merged_data$year <- as.numeric(format(full_merged_data$date, "%Y"))

# Make sure the 'year' column in region_age is numeric
region_age$year <- as.numeric(region_age$year)
# Identify the most recent year in region_age
most_recent_year <- max(region_age$year)
# Adjust region_age to include entries for future years, if needed
if(max(full_merged_data$year) > most_recent_year) {
  # Identify the years that need to be added
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  # Duplicate the most recent year data for each missing year
  recent_year_data <- region_age %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  # Combine with the original region_age data
  region_age <- bind_rows(region_age, future_data)
}
# Create a dataframe for all combinations of region and all dates in full_merged_data
region_dates <- expand.grid(region = unique(region_age$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
# Join expanded_region_age with region_age to fill in the ages_per_1k data
expanded_region_age <- merge(region_dates, region_age, by = c("region", "year"), all.x = TRUE)
# Arrange by region and date for clarity
expanded_region_age <- expanded_region_age %>% arrange(region, date)

most_recent_year_in_years <- max(years)

expanded_region_age$source_year <- expanded_region_age$year
expanded_region_age$adjusted_for_future <- expanded_region_age$year > most_recent_year_in_years
expanded_region_age$source_year <- ifelse(expanded_region_age$year > most_recent_year_in_years, most_recent_year, expanded_region_age$year)
expanded_region_age <- expanded_region_age %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column

expanded_region_age <- expanded_region_age %>%
  select(
    date, region, source_year, "0", "1-4", "5-9", "10-14", "15-19", "20-24",
    "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
    "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85"
  )


########## region_income_raw processing

{
  names(region_income_raw) <- c("region", 2020, 2021, 2022)
  
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_income_raw)[2:ncol(region_income_raw)])))
  
  names(region_income_raw) <- c("region", "income", "income", "income")
  
  region_income_raw <- region_income_raw[-1, ]
  
  region_income_2020 <- region_income_raw[1:2]
  region_income_2021 <- region_income_raw[c(1,3)]
  region_income_2022 <- region_income_raw[c(1,4)]
  
  region_income_2020$date <- years[1]
  region_income_2021$date <- years[2]
  region_income_2022$date <- years[3]
  
  region_income <- rbind(region_income_2020, region_income_2021, region_income_2022)
  
  names(region_income) <- c("region", "income", "year")
  
  region_income <- region_income %>%
    select(
      year, region, income
    )
}

region_income$year <- as.numeric(region_income$year)
most_recent_year <- max(region_income$year)
if(max(full_merged_data$year) > most_recent_year) {
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  recent_year_data <- region_income %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  region_income <- bind_rows(region_income, future_data)
}
region_dates <- expand.grid(region = unique(region_income$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
expanded_region_income <- merge(region_dates, region_income, by = c("region", "year"), all.x = TRUE)
expanded_region_income <- expanded_region_income %>% arrange(region, date)

# # Identify the most recent year in your 'years' vector
most_recent_year_in_years <- max(years)

# This column will simply copy the year for now, but it's set up for future adjustments if needed
expanded_region_income$source_year <- expanded_region_income$year
expanded_region_income$adjusted_for_future <- expanded_region_income$year > most_recent_year_in_years
expanded_region_income$source_year <- ifelse(expanded_region_income$year > most_recent_year_in_years, most_recent_year, expanded_region_income$year)
expanded_region_income <- expanded_region_income %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column
expanded_region_income <- expanded_region_income %>%
  select(
    date, region, source_year, income
  )


########## region smoking processing
{
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_smoking_raw)[2:ncol(region_smoking_raw)])))
  
  names(region_smoking_raw) <- c("region","smoking_rate", "smoking_rate", "smoking_rate", "smoking_rate")
  
  region_smoking_2020 <- region_smoking_raw[1:2]
  region_smoking_2021 <- region_smoking_raw[c(1,3)]
  region_smoking_2022 <- region_smoking_raw[c(1,4)]
  region_smoking_2023 <- region_smoking_raw[c(1,5)]
  
  region_smoking_2020$date <- years[1]
  region_smoking_2021$date <- years[2]
  region_smoking_2022$date <- years[3]
  region_smoking_2023$date <- years[4]
  
  region_smoking <- rbind(region_smoking_2020, region_smoking_2021, region_smoking_2022, region_smoking_2023)
  
  region_smoking$region[region_smoking$region == '강원특별자치도'] <- '강원도'
  
  names(region_smoking) <- c("region", "smoking_rate", "year")
  
  region_smoking <- region_smoking %>%
    select(
      year, region, smoking_rate
    )
}

region_smoking$year <- as.numeric(region_smoking$year)
most_recent_year <- max(region_smoking$year)
if(max(full_merged_data$year) > most_recent_year) {
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  recent_year_data <- region_smoking %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  region_smoking <- bind_rows(region_smoking, future_data)
}
region_dates <- expand.grid(region = unique(region_smoking$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
expanded_region_smoking <- merge(region_dates, region_smoking, by = c("region", "year"), all.x = TRUE)
expanded_region_smoking <- expanded_region_smoking %>% arrange(region, date)

# This column will simply copy the year for now, but it's set up for future adjustments if needed
expanded_region_smoking$source_year <- expanded_region_smoking$year
expanded_region_smoking$adjusted_for_future <- expanded_region_smoking$year > most_recent_year_in_years
expanded_region_smoking$source_year <- ifelse(expanded_region_smoking$year > most_recent_year_in_years, most_recent_year, expanded_region_smoking$year)
expanded_region_smoking <- expanded_region_smoking %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column
expanded_region_smoking <- expanded_region_smoking %>%
  select(
    date, region, source_year, smoking_rate
  )



# region_bed data processing
{
  # years <- unique(sub("^(\\d{4}).*$", "\\1", names(region_bed_raw)[-1]))
  # colnames(region_bed_raw) <- as.character(region_bed_raw[1, ])
  # region_bed <- region_bed_raw[-1, ]  # Remove the first row
  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_bed_raw)[2:ncol(region_bed_raw)])))
  
  colnames(region_bed_raw) <- as.character(region_bed_raw[1, ])
  region_bed_raw <- region_bed_raw[-c(1,2), ]
  
  region_bed_2020 <- region_bed_raw[1:4]
  region_bed_2021 <- region_bed_raw[c(1,5:7)]
  region_bed_2022 <- region_bed_raw[c(1,8:10)]
  
  region_bed_2020$date <- years[1]
  region_bed_2021$date <- years[2]
  region_bed_2022$date <- years[3]
  
  region_bed <- rbind(region_bed_2020, region_bed_2021, region_bed_2022)
  
  names(region_bed) <- c("region","beds_per_1k", "total_beds", "population", "year")
  
  region_bed$region[region_bed$region == '강원특별자치도'] <- '강원도'
  
  region_bed <- region_bed %>%
    select(
      year, region, beds_per_1k
    )
}

# Ensure the 'date' column in full_merged_data is of Date type and extract the year
full_merged_data$date <- as.Date(full_merged_data$date)
full_merged_data$year <- as.numeric(format(full_merged_data$date, "%Y"))
# Make sure the 'year' column in region_bed is numeric
region_bed$year <- as.numeric(region_bed$year)
# Identify the most recent year in region_bed
most_recent_year <- max(region_bed$year)
# Adjust region_bed to include entries for future years, if needed
if(max(full_merged_data$year) > most_recent_year) {
  # Identify the years that need to be added
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)

  # Duplicate the most recent year data for each missing year
  recent_year_data <- region_bed %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)

  # Combine with the original region_bed data
  region_bed <- bind_rows(region_bed, future_data)
}
# Create a dataframe for all combinations of region and all dates in full_merged_data
region_dates <- expand.grid(region = unique(region_bed$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
# Join expanded_region_bed with region_bed to fill in the beds_per_1k data
expanded_region_bed <- merge(region_dates, region_bed, by = c("region", "year"), all.x = TRUE)
# Arrange by region and date for clarity
expanded_region_bed <- expanded_region_bed %>% arrange(region, date)

# # Identify the most recent year in your 'years' vector
# most_recent_year_in_years <- max(years)
# # Adjust expanded_region_bed$year to ensure no year goes beyond the most recent year in 'years'
# expanded_region_bed$year <- ifelse(expanded_region_bed$year > most_recent_year_in_years, most_recent_year_in_years, expanded_region_bed$year)

# Add or update the source_year column in expanded_region_bed
# This column will simply copy the year for now, but it's set up for future adjustments if needed
expanded_region_bed$source_year <- expanded_region_bed$year
# Optional: If you want to explicitly mark rows that were adjusted to use the most recent year's data
expanded_region_bed$adjusted_for_future <- expanded_region_bed$year > most_recent_year_in_years
# Now, adjust the source_year for entries beyond your dataset's range
# Assuming that the years in 'expanded_region_bed' are already capped at the most recent year from 'years'
expanded_region_bed$source_year <- ifelse(expanded_region_bed$year > most_recent_year_in_years, most_recent_year, expanded_region_bed$year)
# Ensure the order is correct and clean up the dataframe
expanded_region_bed <- expanded_region_bed %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column

expanded_region_bed <- expanded_region_bed %>%
  select(
    date, region, source_year, beds_per_1k
  )


# region_doctor data processing
{

  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_doctor_raw)[2:ncol(region_doctor_raw)])))
  
  colnames(region_doctor_raw) <- as.character(region_doctor_raw[1, ])
  region_doctor_raw <- region_doctor_raw[-c(1,2), ]
  
  region_doctor_2020 <- region_doctor_raw[1:4]
  region_doctor_2021 <- region_doctor_raw[c(1,5:7)]
  region_doctor_2022 <- region_doctor_raw[c(1,8:10)]
  region_doctor_2023 <- region_doctor_raw[c(1,11:13)]
  
  region_doctor_2020$date <- years[1]
  region_doctor_2021$date <- years[2]
  region_doctor_2022$date <- years[3]
  region_doctor_2023$date <- years[4]
  
  region_doctor <- rbind(region_doctor_2020, region_doctor_2021, region_doctor_2022, region_doctor_2023)
  
  names(region_doctor) <- c("region", "doctors_per_1k", "total_doctors", "population", "year")
  
  region_doctor$region[region_doctor$region == '강원특별자치도'] <- '강원도'
  
  region_doctor <- region_doctor %>%
    select(
      year, region, doctors_per_1k
    )
  }

# Make sure the 'year' column in region_doctor is numeric
region_doctor$year <- as.numeric(region_doctor$year)
# Identify the most recent year in region_doctor
most_recent_year <- max(region_doctor$year)
# Adjust region_doctor to include entries for future years, if needed
if(max(full_merged_data$year) > most_recent_year) {
  # Identify the years that need to be added
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  # Duplicate the most recent year data for each missing year
  recent_year_data <- region_doctor %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  # Combine with the original region_doctor data
  region_doctor <- bind_rows(region_doctor, future_data)
}

# Create a dataframe for all combinations of region and all dates in full_merged_data
region_dates <- expand.grid(region = unique(region_doctor$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
# Join expanded_region_bed with region_doctor to fill in the doctors_per_1k data
expanded_region_doctor <- merge(region_dates, region_doctor, by = c("region", "year"), all.x = TRUE)
# Arrange by region and date for clarity
expanded_region_doctor <- expanded_region_doctor %>% arrange(region, date)

expanded_region_doctor$source_year <- expanded_region_doctor$year
expanded_region_doctor$adjusted_for_future <- expanded_region_doctor$year > most_recent_year_in_years
expanded_region_doctor$source_year <- ifelse(expanded_region_doctor$year > most_recent_year_in_years, most_recent_year, expanded_region_doctor$year)
expanded_region_doctor <- expanded_region_doctor %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) 

expanded_region_doctor <- expanded_region_doctor %>%
  select(
    date, region, source_year, doctors_per_1k
  )



################# SENIOR ##################
# region_senior data processing
{
  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_senior_raw)[2:ncol(region_senior_raw)])))
  
  colnames(region_senior_raw) <- as.character(region_senior_raw[1, ])
  region_senior_raw <- region_senior_raw[-c(1,2), ]
  
  region_senior_2020 <- region_senior_raw[1:4]
  region_senior_2021 <- region_senior_raw[c(1,5:7)]
  region_senior_2022 <- region_senior_raw[c(1,8:10)]
  region_senior_2023 <- region_senior_raw[c(1,11:13)]
  
  region_senior_2020$date <- years[1]
  region_senior_2021$date <- years[2]
  region_senior_2022$date <- years[3]
  region_senior_2023$date <- years[4]
  
  region_senior <- rbind(region_senior_2020, region_senior_2021, region_senior_2022, region_senior_2023)
  
  names(region_senior) <- c("region", "seniors_per_1k", "total_seniors", "population", "year")
  
  region_senior$region[region_senior$region == '강원특별자치도'] <- '강원도'
  
  region_senior <- region_senior %>%
    select(
      year, region, seniors_per_1k
    )
}

# Make sure the 'year' column in region_senior is numeric
region_senior$year <- as.numeric(region_senior$year)
# Identify the most recent year in region_senior
most_recent_year <- max(region_senior$year)
# Adjust region_senior to include entries for future years, if needed
if(max(full_merged_data$year) > most_recent_year) {
  # Identify the years that need to be added
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  # Duplicate the most recent year data for each missing year
  recent_year_data <- region_senior %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  # Combine with the original region_senior data
  region_senior <- bind_rows(region_senior, future_data)
}

# Create a dataframe for all combinations of region and all dates in full_merged_data
region_dates <- expand.grid(region = unique(region_senior$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
# Join expanded_region_bed with region_senior to fill in the seniors_per_1k data
expanded_region_senior <- merge(region_dates, region_senior, by = c("region", "year"), all.x = TRUE)
# Arrange by region and date for clarity
expanded_region_senior <- expanded_region_senior %>% arrange(region, date)

expanded_region_senior$source_year <- expanded_region_senior$year
expanded_region_senior$adjusted_for_future <- expanded_region_senior$year > most_recent_year_in_years
expanded_region_senior$source_year <- ifelse(expanded_region_senior$year > most_recent_year_in_years, most_recent_year, expanded_region_senior$year)
expanded_region_senior <- expanded_region_senior %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) 

expanded_region_senior <- expanded_region_senior %>%
  select(
    date, region, source_year, seniors_per_1k
  )



###################### region_foreigner data processing
{
  # Extracting unique years from column names
  years <- unique(as.numeric(sub("^([0-9]+)\\.\\.\\..*", "\\1", colnames(region_foreigner_raw)[2:ncol(region_foreigner_raw)])))
  
  colnames(region_foreigner_raw) <- as.character(region_foreigner_raw[1, ])
  region_foreigner_raw <- region_foreigner_raw[-c(1,2), ]
  
  region_foreigner_2020 <- region_foreigner_raw[1:4]
  region_foreigner_2021 <- region_foreigner_raw[c(1,5:7)]
  region_foreigner_2022 <- region_foreigner_raw[c(1,8:10)]
  
  region_foreigner_2020$date <- years[1]
  region_foreigner_2021$date <- years[2]
  region_foreigner_2022$date <- years[3]
  
  region_foreigner <- rbind(region_foreigner_2020, region_foreigner_2021, region_foreigner_2022)
  
  names(region_foreigner) <- c("region","foreigners_per_1k", "total_foreigners", "population", "year")
  
  region_foreigner$region[region_foreigner$region == '강원특별자치도'] <- '강원도'
  
  region_foreigner <- region_foreigner %>%
    select(
      year, region, foreigners_per_1k
    )
  }

region_foreigner$year <- as.numeric(region_foreigner$year)
most_recent_year <- max(region_foreigner$year)
if(max(full_merged_data$year) > most_recent_year) {
  missing_years <- (most_recent_year + 1):max(full_merged_data$year)
  
  recent_year_data <- region_foreigner %>% filter(year == most_recent_year)
  future_data <- lapply(missing_years, function(yr) mutate(recent_year_data, year = yr))
  future_data <- bind_rows(future_data)
  
  region_foreigner <- bind_rows(region_foreigner, future_data)
}
region_dates <- expand.grid(region = unique(region_foreigner$region), date = unique(full_merged_data$date))
region_dates$year <- as.numeric(format(region_dates$date, "%Y"))
expanded_region_foreigner <- merge(region_dates, region_foreigner, by = c("region", "year"), all.x = TRUE)
expanded_region_foreigner <- expanded_region_foreigner %>% arrange(region, date)

# This column will simply copy the year for now, but it's set up for future adjustments if needed
expanded_region_foreigner$source_year <- expanded_region_foreigner$year
expanded_region_foreigner$adjusted_for_future <- expanded_region_foreigner$year > most_recent_year_in_years
expanded_region_foreigner$source_year <- ifelse(expanded_region_foreigner$year > most_recent_year_in_years, most_recent_year, expanded_region_foreigner$year)
expanded_region_foreigner <- expanded_region_foreigner %>%
  arrange(region, date) %>%
  select(-adjusted_for_future) # Remove this line if you want to keep the 'adjusted_for_future' column
expanded_region_foreigner <- expanded_region_foreigner %>%
  select(
    date, region, source_year, foreigners_per_1k
  )

################################################################################

# # Make date index for each dataframe
# {
#   region_sex_index <- data.frame(
#     year = expanded_region_sex$date,
#     source_year = expanded_region_sex$source_year
#   )
#   region_doctor_index <- data.frame(
#     year = expanded_region_doctor$date,
#     source_year = expanded_region_doctor$source_year
#   )
#   region_age_index <- data.frame(
#     year = expanded_region_age$date,
#     source_year = expanded_region_age$source_year
#   )
#   region_foreigner_index <- data.frame(
#     year = expanded_region_foreigner$date,
#     source_year = expanded_region_foreigner$source_year
#   )
#   region_income_index <- data.frame(
#     year = expanded_region_income$date,
#     source_year = expanded_region_income$source_year
#   )
#   region_senior_index <- data.frame(
#     year = expanded_region_senior$date,
#     source_year = expanded_region_senior$source_year
#   )
#   region_smoking_index <- data.frame(
#     year = expanded_region_smoking$date,
#     source_year = expanded_region_smoking$source_year
#   )
#   region_bed_index <- data.frame(
#     year = expanded_region_bed$date,
#     source_year = expanded_region_bed$source_year
#   )
# }

{
  region_sex_df <- expanded_region_sex
  region_sex_df$source_year <- NULL
  
  region_doctor_df <- expanded_region_doctor
  region_doctor_df$source_year <- NULL
  
  region_age_df <- expanded_region_age
  region_age_df$source_year <- NULL
  
  region_foreigner_df <- expanded_region_foreigner
  region_foreigner_df$source_year <- NULL
  
  region_income_df <- expanded_region_income
  region_income_df$source_year <- NULL
  
  region_senior_df <- expanded_region_senior
  region_senior_df$source_year <- NULL
  
  region_smoking_df <- expanded_region_smoking
  region_smoking_df$source_year <- NULL
  
  region_bed_df <- expanded_region_bed
  region_bed_df$source_year <- NULL
}

################################################################################

# save(full_merged_data, 
#      file = "C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/full_merged_data.rdata")


full_merged_sociodemo <- full_merged_data %>%
  select(
    -cum_cases, -cum_deaths, -daily_cases, -per_100k, -daily_deaths,
    -total_cases, -prop_cases, -total_deaths, -prop_deaths, -avg_monthly_cases,
    -region_cases, -avg_monthly_deaths, -region_deaths, -per_100k_cases, -per_100k_deaths
  )


# change region name from Korean to English
expanded_region_bed$region <- region_match$eng[match(expanded_region_bed$region, region_match$kor)]
expanded_region_age$region <- region_match$eng[match(expanded_region_age$region, region_match$kor)]
expanded_region_doctor$region <- region_match$eng[match(expanded_region_doctor$region, region_match$kor)]
expanded_region_foreigner$region <- region_match$eng[match(expanded_region_foreigner$region, region_match$kor)]
expanded_region_income$region <- region_match$eng[match(expanded_region_income$region, region_match$kor)]
expanded_region_senior$region <- region_match$eng[match(expanded_region_senior$region, region_match$kor)]
expanded_region_sex$region <- region_match$eng[match(expanded_region_sex$region, region_match$kor)]
expanded_region_smoking$region <- region_match$eng[match(expanded_region_smoking$region, region_match$kor)]

# Sum age group into larger groups
expanded_region_age2 <- expanded_region_age
age_columns <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85")
expanded_region_age2[age_columns] <- lapply(expanded_region_age[age_columns], as.numeric)
# Aggregate age ranges
expanded_region_age2$age_0_24 <- rowSums(expanded_region_age2[, c("0", "1-4", "5-9", "10-14", "15-19", "20-24")])
expanded_region_age2$age_25_64 <- rowSums(expanded_region_age2[, c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")])
expanded_region_age2$age_65_plus <- rowSums(expanded_region_age2[, c("65-69", "70-74", "75-79", "80-84", "above 85")])
# Remove original age columns
expanded_region_age2 <- expanded_region_age2[, !(names(expanded_region_age2) %in% age_columns)]







# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_bed, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_age, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_doctor, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_foreigner, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_income, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_senior, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_sex, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo <- merge(full_merged_sociodemo, expanded_region_smoking, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)





# change region name from Korean to English
region_sex_df$region <- region_match$eng[match(region_sex_df$region, region_match$kor)]
region_doctor_df$region <- region_match$eng[match(region_doctor_df$region, region_match$kor)]
region_age_df$region <- region_match$eng[match(region_age_df$region, region_match$kor)]
region_foreigner_df$region <- region_match$eng[match(region_foreigner_df$region, region_match$kor)]
region_income_df$region <- region_match$eng[match(region_income_df$region, region_match$kor)]
region_senior_df$region <- region_match$eng[match(region_senior_df$region, region_match$kor)]
region_smoking_df$region <- region_match$eng[match(region_smoking_df$region, region_match$kor)]
region_bed_df$region <- region_match$eng[match(region_bed_df$region, region_match$kor)]

# Sum age group into larger groups
region_age2_df <- region_age_df
age_columns <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "above 85")
region_age2_df[age_columns] <- lapply(region_age2_df[age_columns], as.numeric)
# Aggregate age ranges
region_age2_df$age_0_24 <- rowSums(region_age2_df[, c("0", "1-4", "5-9", "10-14", "15-19", "20-24")])
region_age2_df$age_25_64 <- rowSums(region_age2_df[, c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")])
region_age2_df$age_65_plus <- rowSums(region_age2_df[, c("65-69", "70-74", "75-79", "80-84", "above 85")])
# Remove original age columns
region_age2_df <- region_age2_df[, !(names(region_age2_df) %in% age_columns)]


full_merged_sociodemo_df <- full_merged_sociodemo
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_sex_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_doctor_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
# full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_age_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_age2_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_foreigner_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_income_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_senior_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_smoking_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)
full_merged_sociodemo_df <- merge(full_merged_sociodemo_df, region_bed_df, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE)

save(full_merged_sociodemo_df,
     file = "C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/full_merged_sociodemo_df.rdata")



# change all the values into year
exclude_cols <- c("date", "region")
expanded_region_bed_y <- expanded_region_bed
expanded_region_bed_y[, !(names(expanded_region_bed) %in% exclude_cols)] <- expanded_region_bed$source_year
# expanded_region_age_y <- expanded_region_age
# expanded_region_age_y[, !(names(expanded_region_age) %in% exclude_cols)] <- expanded_region_age$source_year
expanded_region_doctor_y <- expanded_region_doctor
expanded_region_doctor_y[, !(names(expanded_region_doctor) %in% exclude_cols)] <- expanded_region_doctor$source_year
expanded_region_foreigner_y <- expanded_region_foreigner
expanded_region_foreigner_y[, !(names(expanded_region_foreigner) %in% exclude_cols)] <- expanded_region_foreigner$source_year
expanded_region_income_y <- expanded_region_income
expanded_region_income_y[, !(names(expanded_region_income) %in% exclude_cols)] <- expanded_region_income$source_year
expanded_region_senior_y <- expanded_region_senior
expanded_region_senior_y[, !(names(expanded_region_senior) %in% exclude_cols)] <- expanded_region_senior$source_year
expanded_region_sex_y <- expanded_region_sex
expanded_region_sex_y[, !(names(expanded_region_sex) %in% exclude_cols)] <- expanded_region_sex$source_year
expanded_region_smoking_y <- expanded_region_smoking
expanded_region_smoking_y[, !(names(expanded_region_smoking) %in% exclude_cols)] <- expanded_region_smoking$source_year
expanded_region_age2_y <- expanded_region_age2
expanded_region_age2_y[, !(names(expanded_region_age2) %in% exclude_cols)] <- expanded_region_age2$source_year

full_merged_sociodemo_year <- full_merged_sociodemo
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_bed_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_bed"))
# full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_age_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_age"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_age2_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_age"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_doctor_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_doctor"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_foreigner_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_foreigner"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_income_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_income"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_senior_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_senior"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_sex_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_sex"))
full_merged_sociodemo_year <- merge(full_merged_sociodemo_year, expanded_region_smoking_y, by.x = c("name", "date"), by.y = c("region", "date"), all.x = TRUE, suffixes = c("", "_smoking"))

save(full_merged_sociodemo_year, file = "C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/full_merged_sociodemo_year.rdata")







# save(full_merged_data, 
#      file = "C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/full_merged_data.rdata")

# demo_data_list <- list(bed = list(type = "Single", data = region_bed_df, ref = region_bed_index),
#                        smoking = list(type = "Single", data = region_smoking_df, ref = region_smoking_index),
#                        senior = list(type = "Single", data = region_senior_df, ref = region_senior_index),
#                        income = list(type = "Single", data = region_income_df, ref = region_income_index),
#                        foreigner = list(type = "Single", data = region_foreigner_df, ref = region_foreigner_index),
#                        age = list(type = "Grouped", data = region_age_df, ref = region_age_index),
#                        doctor = list(type = "Single", data = region_doctor_df, ref = region_doctor_index),
#                        sex = list(type = "Grouped", data = region_sex_df, ref = region_sex_index))
# save(demo_data_list, 
#      file = "
# C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/sociodemo.rdata")

# save(region_sex_index, region_doctor_index, region_age_index, region_foreigner_index, 
#      region_income_index, region_senior_index, region_smoking_index,region_sex_df, region_doctor_df,region_age_df,
#      region_foreigner_df,region_income_df, region_senior_df, region_smoking_df,region_bed_df, 
#      file = "C:/Users/ron18/Desktop/2022-1/bibs/covid19/kinfectdistrack/Korea viz/r code/sociodemo.rdata")