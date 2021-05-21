# Read the Table of Estimates from the Selected Models
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 1: Create Function

####********************
#### D: Description ####
####********************

# This function allows us to pull in the effect estimates for just 
# the model with the selected constraints

####************************
#### 1: Create Function ####
####************************

# 1a Name function
read_estimates <- function(Sensitivity, SubSetVar, SubSet,
                                   ERConstraint, LRConstraint, IndCumul){
  #  Sensitivity <- 'Main'; SubSetVar <- 'FullSet'; SubSet <- 'FullSet'; 
  # IndCumul <- 'EstInd' # IndCumul can be 'EstInd' or 'EstCumul'

  # 1b Identify the selected model constraints if appropriate
  if(ERConstraint == 'Selected' & LRConstraint == 'Selected'){
    SelectedModel <- SelectedModels %>% 
      filter(Sensitivity == !!Sensitivity, 
             SubSetVar == !!SubSetVar, SubSet == !!SubSet)
    ERConstraint <- SelectedModel$ERConstraint[1]
    LRConstraint <- SelectedModel$LRConstraint[1]
  }
  # 1c Create ModelNames 
  ModelName <- paste(Sensitivity, SubSetVar, SubSet, 
                     ERConstraint, LRConstraint, sep = '_')  
  # 1d Read predictions of selected models
  # this line will generate the warning 'Missing column names filled in: 'X1' [1]'
  # this warning is just about some nonsense column, probably made by excel. 
  # since this warning shows up a lot, and it is annoying and uninformative 
  # I suppress it here. 
  suppressWarnings(
  est.table <- read_csv(here::here(outPath, 'estimates',
                                   paste0(IndCumul, '_', ModelName, '.csv'))) %>% 
    dplyr::select(-X1) %>% 
    mutate(Sensitivity = !!Sensitivity, 
           SubSetVar = !!SubSetVar, SubSet = !!SubSet, IndCumul = !!IndCumul) 
  )
}
