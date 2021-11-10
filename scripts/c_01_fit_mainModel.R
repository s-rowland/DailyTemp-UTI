# Fit Main Model
# Analysis
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

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

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Begin the timer
tic("main Model Completed")

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the dataset of the matched days with exposure
dta <- read_fst(here::here(
  "data", "prepared",
  paste0("cases_assignedTemp_", outcomeName, ".fst")
))

# 1b Generate set of observed temperatures
tempObs <- dta %>%
  group_by(adate, fips) %>%
  summarize(temp = mean(tLag00))

####***********************
#### 2: Fit Main Model ####
####***********************

# 2a Create table of potential constraints ('candidate constraints')
# here you can decide which constraints to consider.
# With log knots, there are more knots around the lags most proximate to the event
# and fewer knots further away from the event
# so that the curve can change direction more quickly close to the event
# and changes more smoothly further away from the event.

candidateConstraintsGrid <- expand_grid(
  ERConstraint = c("3dfEvenKnots", "4dfEvenKnots", "5dfEvenKnots"), 
  #vdo comment: could we specify ER/LR (ie - is it even __ and log __)?
  #str: sorry, I'm not sure what you mean. the columns are named ERConstraint and 
  # LRConstraint to show what they contain. 
  LRConstraint = c("3dfLogKnots", "4dfLogKnots")
)

# 2b Perform grid search to identify optional constraints
# when we run these models, we do not save the results; only their AIC/QAIC
# note: when using the toy data, you will get a warning that the 'chat'
# is < 1; you can ignore this warning because our goal with the synthetic data
# is to just replicate results
#vdo comment: what other parameters can there be? I'm a bit confused by how I was supposed to know main/fullSet/fullSet were appropriate 
#vdo comment: got it - I see references in c_00a_analyzeTempDLNM (53-67); maybe include those descriptives here for easier understanding or a reference to that particular script
# str: added explanation below 
# 'main' sensitivity is for the main analysis 
# 'fullSet' refers to all female UTI cases - our main case definition
performGridSearch(
  candidateConstraintsGrid = candidateConstraintsGrid, 
  sensitivity = "main", 
  subSetVar = "fullSet", 
  subSet = "fullSet") 

# 2c Get estimates from model with selected constraints
identifySelectedConstraintsFitModel(
  sensitivity = "main", 
  subSetVar = "fullSet", 
  subSet = "fullSet")

# 2d Tell the analyst that the script is done
toc()
