# Assign Exposure
# Data Preparation
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Read Data
# 2: Fit Alternative RH Constraint
# 3: Fit Alternative Lag
# 4: Fit Female-and-Male Model

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
tic("sensitivity Analyses Completed")

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

####**************************************
#### 2: Fit Alternative RH Constraint ####
####**************************************

# 2a Fit no RH model
fitModelWithSelectedMainConstraints("noRH", "fullSet", "fullSet")

# 2b Fit RH DLNM model
fitModelWithSelectedMainConstraints("RHdlnm", "fullSet", "fullSet")

####**********************************
#### 3: Fit Alternative Lag Model ####
####**********************************

# For 21 day lag, we re-select the constraints because we have a different cross-basis
# 3a Make grid of potential parameters we will then evaluate
candidateConstraintsGrid <- expand_grid(
  ERConstraint = c("3dfEvenKnots", "4dfEvenKnots", "5dfEvenKnots"),
  LRConstraint = c("3dfLogKnots", "4dfLogKnots")
)

# 3b Fit 21 day lag model
# 3b.i Perform grid search to identify optional constraints
performGridSearch(candidateConstraintsGrid, "21DayLag", "fullSet", "fullSet")
# 3b.ii Get estimates from model with selected constraints
identifySelectedConstraintsFitModel("21DayLag", "fullSet", "fullSet")

####**********************************
#### 4: Fit Female-and-Male Model ####
####**********************************

# 4a Model with female and male cases
fitModelWithSelectedMainConstraints("FandM", "fullSet", "fullSet")

# 4b Tell the analyst that you are done
toc()
