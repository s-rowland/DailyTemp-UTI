# Assign Exposure
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Read Data 
# 2: Fit Main Model

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

# 0a Tell the analyst that the script is beginning 
StartTime_c_01 <- Sys.time()
print(paste('begin c_01 at', StartTime_c_01))

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the dataset of the matched days with exposure
dta <- read_fst(here::here('data', 'preparedData',
                           paste0('cases_assignedTemp', outcomeName, '.fst')))

# 1b Bring in the daily temperature dataset 
if(user == 'Analyst'){
  temper <- read_fst(here::here('data', 'intermediateData',
                                'daily_weather.fst')) 
}
if(user == 'Reviewer'){
  temper <- read_fst(here::here('data', 'intermediateData',
                                'temperature_fips_fake.fst')) 
}

####*******************
#### 2: Fit Models ####
####*******************

analyze_dlnmTemp('oneLag', 'FullSet', 'FullSet',
                             '3dfevenknots', 'Lag00')