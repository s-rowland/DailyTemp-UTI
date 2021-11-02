# Fit Stratified Models
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
# 2: Fit Seasonal Models
# 3: Fit Catchement Area Models
# 4: Fit ICE Models

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
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Begin the timer
tic("EMM models completed")

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

# 1c Create table of potential constraints ('candidate constraints')
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
  ERConstraint = c("3dfEvenKnots", "4dfEvenKnots", "5dfEvenKnots"),
  LRConstraint = c("3dfLogKnots", "4dfLogKnots")
)

####****************************
#### 2: Fit Seasonal Models ####
####****************************

# 2a Fit winter-only model
fitModelWithSelectedMainConstraints("main", "season", "win")

# 2b Fit spring-only model
fitModelWithSelectedMainConstraints("main", "season", "spr")

# 2c Fit summer-only model
fitModelWithSelectedMainConstraints("main", "season", "sum")

# 2d Fit fall-only model
fitModelWithSelectedMainConstraints("main", "season", "fal")

####***********************************
#### 3: Fit Catchement Area Models ####
####***********************************

# 3a Fit KPSC-only model
fitModelWithSelectedMainConstraints("main", "catchmentArea", "kpsc")

# 3b Fit Sutter-only model
fitModelWithSelectedMainConstraints("main", "catchmentArea", "sutter")

####***********************
#### 4: Fit ICE Models ####
####***********************

# 4a Fit model with UTI of subjects from lowICE tracts
fitModelWithSelectedMainConstraints("main", "ice", "iceLow")

# 4a Fit model with UTI of subjects from highICE tracts
fitModelWithSelectedMainConstraints("main", "ice", "iceHigh")

# 4c Tell the analyst that the emm models are done
toc()
