# Prepare identified cases 
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Wrangle Temperature Data

####**************
#### N: Notes #### 
####**************

####*********************************
#### 1: Wrangle Temperature Data #### 
####*********************************

# 1a Readin Sutter counties dataframe
sutter_counties <- read_csv(here::here('data', 'prepared', 
                                       paste0('sutter_counties', '.csv')))
                           
# 1b Read in temp data
temper <- read_csv("I:/ARES/temperature/temp_2015_2017.csv")

# 1c Fix temp fips
temper$fips <- substr(temper$fips, 2,5)
head(temper$fips)
temper$fips <- as.numeric(temper$fips)

# 1d Convert string to date for temperature
temper <- temper %>% 
  mutate(adate = as.Date(date, format="%d/%m/%Y")) %>% 
  dplyr::select(fips, tmean, adate)

# 1e Read in rh data
# rh data is the average RH over the catchment area
rh <- read_csv("I:/ARES/temperature/rh_cali_2015_2017.csv")

# 1f Fix rh dates
rh <- rh %>% mutate(date = as.Date(date, format="%m/%d/%y")) 

# 1g Join temperature and sutter counties
temper <- temper %>% 
  left_join(sutter_counties, by=c("fips"="COUNTYFP")) %>% 
  mutate(kp = if_else(is.na(sutter_county),1,0))

# 1h Merge temp & rh
temper2 <- left_join(temper, rh, by=c("adate"="date", "kp"="kp"))
temper2 <- temper2 %>%
  dplyr::select(-kp, -NAMELSAD, -sutter_county)

# 1i Save data 
# I use the fst format because it saves memory and it faster to read/write
temper2 %>% 
  write_fst(here::here('data', 'intermediate', 
                       'daily_weather.fst'))
