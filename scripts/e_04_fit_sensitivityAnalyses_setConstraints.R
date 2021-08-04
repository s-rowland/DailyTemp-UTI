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
# 2: Fit Alternative Lag 

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

####****************************
#### 3: Fit Alternative Lag ####
####****************************

# force even knots
analyze_dlnmTemp('Main', 'FullSet', 'FullSet',
                 '3dfevenknots', '3dfevenknots', 'SaveModel')
analyze_dlnmTemp('14DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', '3dfevenknots', 'SaveModel')
analyze_dlnmTemp('21DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', '3dfevenknots', 'SaveModel')

# force logknots
analyze_dlnmTemp('Main', 'FullSet', 'FullSet',
                 '3dfevenknots', '4dflogknots', 'SaveModel')
analyze_dlnmTemp('14DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', '4dflogknots', 'SaveModel')
analyze_dlnmTemp('21DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', '4dflogknots', 'SaveModel')

# force penalized spline   
analyze_dlnmTemp('14DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', 'psp', 'SaveModel')
analyze_dlnmTemp('21DayLag', 'FullSet', 'FullSet',
                 '3dfevenknots', 'psp', 'SaveModel')
