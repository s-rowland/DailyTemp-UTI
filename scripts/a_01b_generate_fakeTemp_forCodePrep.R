# Create Fake Temperature Data for Code Testing
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Fake Temperature Data

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to create a fake dataset for reproducibility testing
# this fake dataset should have a null association between temp and UTI

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####*************************************
#### 1: Create Fake Temperature Data ####
####*************************************

# 1a Set number of years of fake data 
NDays <- 365 + 366 + 365

# 1b Create a vector of fips Codes
activefips <- c(6001, 6003, 6005, 6006, 6007, 6008, 6009)

# 1c Begin dataset with Index variable
temper <- expand_grid(
  dIndex = c(1:NDays),
  fips = activefips)

# 1d Create date variable
# this variable is the ADMDT variable, but in datetime format 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
temper <- temper %>%  
  mutate(adate = parse_date_time('1/1/1999', 'mdy', tz = 'America/Los_Angeles') + 
                                dIndex*60*60*24) %>% 
  mutate(adate = str_sub(str_remove_all(adate, '-'), 0, 8))

# 1e Create temperature variable 
set.seed(1234)
temper$tmean <- rnorm(nrow(temper), 10 + 10* sin(2*pi*temper$dIndex / 366), 2)
temper <- temper %>% 
  dplyr::select(-contains('Index'))

# 1f Add RH 
temper$avgrelhum <- rnorm(nrow(temper), 0, 10)

# 1g Save data 
temper %>% 
  write_fst(here::here('data', 'intermediateData',
                       'fake_weather.fst'))

# 1h Clean up environment 
rm(activefips, temper, NDays)
