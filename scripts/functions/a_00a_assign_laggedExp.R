# Assign Lagged Exposures
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Define Function to Assign Lagged Exposure 

####**************
#### N: Notes #### 
####**************

####**************************************************
#### 1: Define Function to Assign Lagged Exposure ####
####**************************************************

# 1a Name function 
assign_laggedExp <- function(dta_outcome, dta_exp, numLag){
  # dta_outcome <- dta_assignedTemp; dta_exp <- temp; numLag <- 3
  
  # 1b Create variable name 
  VarName <- paste0('tLag', str_pad(numLag, 2, 'left', '0'))
  
  # 1c Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(activeLag := ADMDateTime - as.period(1 * numLag, 'day'))
  
  # 1d Join with exposure data 
  dta_outcome <- dta_exp %>% 
    dplyr::select(fips, activeLag, tmean) %>%
    inner_join(dta_outcome, by = c('fips', 'activeLag'))
  
  # 1e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName := tmean) %>% 
    dplyr::select(-tmean, -activeLag)
}
  