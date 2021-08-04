# Identify the Selected Constraints and Fit the Model with those Constraints
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Make Function to Fit a Model with the Selected Constraints

####**************
#### N: Notes #### 
####**************

# Na Description
# This is a very simple function; as a function we can make adjustments to this code 
# to modify each time we fit a selected model. 
# this is most useful if we are running multiple subgroup analyses. 

####********************
#### 0: Preparation #### 
####********************

####*******************************************************************
#### 1: Make Function to Fit a Model with the Selected Constraints #### 
####*******************************************************************

# 1a Begin function  
identifySelectedConstraints_fitModel <- function(Sensitivity, SubSetVar, SubSet){
  # Sensitivity <- 'Main'; SubSetVar <- 'FullSet'; Subset <- 'FullSet'
  
  # 1b Read all of the selected models 
  # and keep only the selected constraints for the model of interest
  SelectedModel <- read_csv(here::here(outPath, 'tables',
                                       'SelectedModels.csv')) %>% 
    filter(Sensitivity == !!Sensitivity & 
             SubSetVar == !!SubSetVar & SubSetVar == !!SubSetVar)
  
  # 1c Fit and store selected model
  analyze_dlnmTemp(Sensitivity, SubSetVar, SubSet,
                SelectedModel$ERConstraint[1], SelectedModel$LRConstraint[1], 
                'SaveModel')
}
