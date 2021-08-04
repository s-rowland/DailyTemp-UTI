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
# 2: Fit Alternative RH Constraint
# 3: Fit Alternative Lag 

####**************
#### N: Notes #### 
####**************


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
if(outcomeName == 'UTI'){
  temper <- read_fst(here::here('data', 'intermediateData', 'daily_weather.fst')) 
}
if(outcomeName == 'fake'){
  temper <- read_fst(here::here('data', 'intermediateData', 'fake_weather.fst')) 
}

# 1c Get the selected constraints of the main model 
SelectedModel <- read_csv(here::here(outPath, 'tables',
                                     'SelectedModels.csv')) %>% 
  filter(Sensitivity == 'Main' & SubSetVar == 'FullSet' & SubSetVar == 'FullSet')

####**************************************
#### 2: Fit Alternative RH Constraint ####
####**************************************

# 2a fit no RH model
analyze_dlnmTemp('noRH', 'FullSet', 'FullSet',
                 SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1],
                 'SaveModel')
# 2b fit no RH model
analyze_dlnmTemp('RHdlnm', 'FullSet', 'FullSet',
                 SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1],
                 'SaveModel')

## code if we want to select constraints for each sensitivity analysis ##
# 2a.i Perform grid search to identify optional constraints
#perform_gridSearch(candidateConstraintsGrid, 'noRH', 'FullSet', 'FullSet')
# 2a.ii Get estimates from model with selected constraints
#identifySelectedConstraints_fitModel('noRH', 'FullSet', 'FullSet')

# 2b fit RH dlnm model
# 2b.i Perform grid search to identify optional constraints
#perform_gridSearch(candidateConstraintsGrid, 'RHdlnm', 'FullSet', 'FullSet')
# 2b.ii Get estimates from model with selected constraints
#identifySelectedConstraints_fitModel('RHdlnm', 'FullSet', 'FullSet')

####****************************
#### 3: Fit Alternative Lag ####
####****************************

# 3a Make grid of potential parameters we will then evaluate 
candidateConstraintsGrid <- expand_grid(
  ERConstraint = c('lin', '3dfevenknots','4dfevenknots', '5dfevenknots'),
  LRConstraint = c('3dflogknots', '4dflogknots'))

# 3b fit 14 day Lag model
# 3b.i Perform grid search to identify optional constraints
perform_gridSearch(candidateConstraintsGrid, '7DayLag', 'FullSet', 'FullSet')
# 3b.ii Get estimates from model with selected constraints
identifySelectedConstraints_fitModel('7DayLag', 'FullSet', 'FullSet')

# 3c fit 21 day Lag model
# 3c.i Perform grid search to identify optional constraints
perform_gridSearch(candidateConstraintsGrid, '21DayLag', 'FullSet', 'FullSet')
# 3c.ii Get estimates from model with selected constraints
identifySelectedConstraints_fitModel('21DayLag', 'FullSet', 'FullSet')
