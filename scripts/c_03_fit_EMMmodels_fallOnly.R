# Fit Stratified Models
# Analysis
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Read Data
# 2: Fit Seasonal Models
# 3: Fit Catchement Area Models
# 4: Fit ICE Models

####**************
#### N: Notes ####
####**************

# Na Description
# I am only including code for the parts of the data prep that I've done;
# other data preparation steps could be included in this script, or placed in
# their own script(s).
# I made assumptions about what the other data preparation sctions would be,
# just so that I could have some placeholder section names

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Begin the timer
tic("EMM models completed")

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the dataset of the matched days with exposure
dta <- read_fst(here::here(
  "data", "prepared",
  paste0("cases_assignedTemp_", outcomeName, ".fst")
))

# 1b Generate set of observed temperatures
tempObs <- dta %>%
  group_by(adate, fips) %>%
  summarize(temp = mean(tLag00))

dta <- dta %>% 
  mutate(ADMDateTime = adate)
#mutate(ADMDateTime = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles'))

# 2b Here I make date variables
dta <- dta %>% 
  mutate(MM = month(ADMDateTime), 
         DoW = wday(ADMDateTime)) %>% 
  mutate(season = case_when(
    MM %in% c(12, 1, 2) ~'win', 
    MM %in% c(3, 4, 5) ~'spr', 
    MM %in% c(6, 7, 8) ~'sum', 
    MM %in% c(9, 10, 11) ~'fal'), 
    dow = case_when(
      DoW %in% c(1:5) ~ 'wk', 
      DoW %in% c(6, 7) ~ 'wknd'))

# 1c Restrict to fall
dta <- dta %>% 
  filter(season == 'fal')


####***********************************
#### 3: Fit Catchement Area Models ####
####***********************************

# 3a Fit KPSC-only model
fitModelWithSelectedMainConstraints("fallOnly", "catchmentArea", "kpsc")

# 3b Fit Sutter-only model
fitModelWithSelectedMainConstraints("fallOnly", "catchmentArea", "sutter")

####***********************
#### 4: Fit ICE Models ####
####***********************

# 4a Fit model with UTI of subjects from lowICE tracts
fitModelWithSelectedMainConstraints("fallOnly", "ice", "iceQ1")

# 4a Fit model with UTI of subjects from highICE tracts
fitModelWithSelectedMainConstraints("fallOnly", "ice", "iceQ234")

# 4c Tell the analyst that the emm models are done
toc()
