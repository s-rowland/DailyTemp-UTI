# Create Toy Temperature Data for Code Testing
# Data Preparation
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Create Toy Temperature Data

####**************
#### N: Notes ####
####**************

# Na Description
# The goal of this script is to create a toy dataset for reproducibility testing
# this toy dataset should have a slight positive association between temp and UTI

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Set seed 
set.seed(1234)

####*************************************
#### 1: Create Toy Temperature Data ####
####*************************************

# 1a Set number of years of toy data
# str: added an extra year's worth of days
nDays <- 365 + 365 + 366 + 365

# 1b Create a vector of fips Codes
activefips <- c(6001, 6003, 6005, 6006, 6008, 6009, 6041)

# 1c Begin dataset with Index variable
temper <- expand_grid(
  dIndex = c(1:nDays),
  fips = activefips
)

# 1d Create date variable
# this variable is the ADMDT variable, but in datetime format
# I prefer to keep a column of the date as a string,
# because sometime excel will autocorrect dates to whatever excel thinks is the
# right interpretation.
# str: now temperature data starts at 1998
temper <- temper %>%
  mutate(adate = parse_date_time("1/1/1998", "mdy", tz = "America/Los_Angeles") +
    dIndex * 60 * 60 * 24) %>%
  mutate(adate = str_sub(str_remove_all(adate, "-"), 0, 8))

# 1e Create temperature variable
set.seed(1234)
temper$tmean <- rnorm(nrow(temper), 10 + 10 * sin(2 * pi * temper$dIndex / 366), 2)
temper <- temper %>%
  dplyr::select(-contains("Index"))

# 1f Add RH
temper$avgrelhum <- rnorm(nrow(temper), 0, 10)

# 1g Save data
temper %>%
  write_fst(here::here(
    "data", "intermediate",
    "toy_weather.fst"
  ))

# 1h Clean up environment
rm(activefips, temper, nDays)
