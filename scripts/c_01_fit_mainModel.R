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
temper <- read_fst(here::here('data', 'intermediateData',
                              "temperature_fips_fake.fst")) 

####***********************
#### 2: Fit Main Model ####
####***********************

# 2a Create table of potential constraints ('candidate constraints')
# here you can decide which constraints to consider. 
# In particular, do you want the knots along the lag dimension to be evenly-spaced 
# or evenly-spaced along the log of the lag values ('log knots')? 
# With log knots, there are more knots around the lags most proximate to the event 
# and fewer knots further away from the event 
# so that the curve can change direction more quickly close to the event 
# and changes more smoothly further away from the event. 
# note also that if you choose to use penalized splines for one dimension, 
# you don't need to consider alternative constraints for that dimension

candidateConstraintsGrid <- expand_grid(
  ERConstraint = c('lin', '3dfevenknots','4dfevenknots', '5dfevenknots'),
  LRConstraint = c('3dfevenknots', '4dfevenknots', '5dfevenknots'))

# 2b Perform grid search to identify optional constraints
# when we run these models, we do not save the results; only their AIC/QAIC
perform_gridSearch(candidateConstraintsGrid, 'Main', 'FullSet', 'FullSet')
  
# 2c Get estimates from model with selected constraints
identifySelectedConstraints_fitModel('Main', 'FullSet', 'FullSet')

# 2d Tell the analyst that the script is done
cat('completed c_01 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_c_01), 1), ' min\n')
rm(StartTime_c_01)
