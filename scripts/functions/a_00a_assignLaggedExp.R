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
assignLaggedExp <- function(dtaOutcome, dtaExp, activeLag){
  # dta_outcome <- dta_assignedTemp; dta_exp <- temper; activeLag <- 3
  
  # 1b Create variable name 
  varNameT <- paste0('tLag', str_pad(activeLag, 2, 'left', '0'))
  varNameR <- paste0('rLag', str_pad(activeLag, 2, 'left', '0'))
  
  # 1c Create column of lag 
  dtaOutcome <- dtaOutcome %>% 
    mutate(lagDate := ADMDateTime - as.period(1 * activeLag, 'day'))
  
  # 1d Join with exposure data 
  dtaOutcome <- dtaExp %>% 
    dplyr::select(fips, lagDate, tmean, avgrelhum) %>%
    inner_join(dtaOutcome, by = c('fips', 'lagDate'))
  
  # 1e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dtaOutcome <- dtaOutcome %>% 
    mutate(!!varNameT := tmean, !!varNameR := avgrelhum) %>% 
    dplyr::select(-tmean, -lagDate, -avgrelhum)
}
  