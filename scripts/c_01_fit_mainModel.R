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

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

# 0b Begin the timer 
tic('wknd Model Completed')

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the dataset of the matched days with exposure
dta <- read_fst(here::here('data', 'prepared',
                           paste0('cases_assignedTemp_', outcomeName, '.fst')))

# 1b Generate set of observed temperatures
tempObs <- dta %>% 
  group_by(adate, fips) %>% 
  summarize(temp = mean(tLag00))

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
  ERConstraint = c('3dfEvenKnots','4dfEvenKnots', '5dfEvenKnots'),
  LRConstraint = c('3dfLogKnots', '4dfLogKnots'))

# 2b Perform grid search to identify optional constraints
# when we run these models, we do not save the results; only their AIC/QAIC
performGridSearch(candidateConstraintsGrid, 'main', 'fullSet', 'fullSet')
  
# 2c Get estimates from model with selected constraints
identifySelectedConstraintsFitModel('main', 'fullSet', 'fullSet')

# 2d Tell the analyst that the script is done
toc()
