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
identifySelectedConstraintsFitModel <- function(sensitivity, subSetVar, subSet){
  # sensitivity <- 'main'; subsetVar <- 'season'; subset <- 'sum'
  
  # 1b Read all of the selected models 
  # and keep only the selected constraints for the model of interest
  selectedConstraints <- read_csv(here::here(outPath, 'tables',
                                       'selected_constraints.csv')) %>% 
    filter(sensitivity == !!sensitivity & 
             subSetVar == !!subSetVar & subSet == !!subSet)
  
  # 1c Fit and store selected model
  analyzeTempDLNM(sensitivity, subSetVar, subSet,
                selectedConstraints$ERConstraint[1], selectedConstraints$LRConstraint[1], 
                'saveModel')
}
