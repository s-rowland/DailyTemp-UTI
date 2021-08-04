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
# 2: Fit Model without Offset 
# 3: Fit Model with Offset 
# 4: Compare Models

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
temper <- read_fst(here::here('data', 'intermediateData',
                              "temperature_fips_fake.fst")) 

####*********************************
#### 2: Fit Model without Offset ####
####*********************************

# 2a Convert setID to a factor 
# the gnm() function, which we use for the conditional poission 
# requires the matching variable to have factor format
dta <- dta %>% 
  mutate(setID = as.factor(setID)) %>% 
  arrange(setID)

# 2b Create crossbasis
# 2b.i Isolate the exposure during the lags of interest
exposure_profiles <- as.matrix(dplyr::select(dta, contains('tLag')))[,1:7]
# 2b.ii Make crossbasis matrix
cb.temp <- crossbasis(
  exposure_profiles, 
  lag=c(0,6), # we subtract 1 because lags start at 0
  argvar=list(fun='lin'),
  arglag=list(fun='ns', df = 2))

# 2c Fit model 
mod.noOffset <- gnm(case_count ~ cb.temp, 
           family = quasipoisson(link= 'log'), 
           data = dta, 
           eliminate = setID)

# 2d Generate e
summary(mod.noOffset)$coeff
summary(mod.noOffset)$dispersion

####******************************
#### 3: Fit Model with Offset ####
####******************************

# 3a Fit model 
mod.Offset <- gnm(case_count ~ cb.temp, 
                    family = quasipoisson(link= 'log'), 
                    data = dta, 
                    eliminate = setID, 
                  offset = Pop)

####***********************
#### 4: Compare Models ####
####***********************

# 4a Compare coefficients 
summary(mod.noOffset)$coeff
summary(mod.Offset)$coeff

# 4b Compare dispersion parameter
summary(mod.noOffset)$dispersion
summary(mod.Offset)$dispersion
