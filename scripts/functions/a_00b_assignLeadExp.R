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
assignLeadExp <- function(dtaOutcome, dtaExp, activeLead){
  # dta_outcome <- dta_assignedTemp; dta_exp <- temper; numLag <- 3
  
  # 1b Create variable name 
  varNameT <- paste0('tLead', str_pad(activeLead, 2, 'left', '0'))

  # 1c Create column of lag 
  dtaOutcome <- dtaOutcome %>% 
    mutate(leadDate := ADMDateTime + as.period(1 * activeLead, 'day'))
  
  # 1d Join with exposure data 
  dtaOutcome <- dtaExp %>% 
    dplyr::select(fips, leadDate, tmean, avgrelhum) %>%
    inner_join(dtaOutcome, by = c('fips', 'leadDate'))
  
  # 1e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dtaOutcome <- dtaOutcome %>% 
    mutate(!!varNameT := tmean) %>% 
    dplyr::select(-tmean, -leadDate, -avgrelhum)
}
  