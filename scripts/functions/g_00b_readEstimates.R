# Read the Table of Estimates from the Selected Models
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

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
readEstimates <- function(sensitivity, subSetVar, subSet,
                          ERConstraint, LRConstraint, indCumul, expRange = 'all'){
  
  #  sensitivity <- 'main'; subSetVar <- 'FullSet'; subSet <- 'FullSet'; 
  # ERConstraint <- 'selectedMain'; LRConstraint <- 'selectedMain'
  # indCumul <- 'EstInd'; refT <- '5thPer'
  # indCumul can be 'EstInd' or 'EstCumul'

  # 1b Identify the selected model constraints if appropriate
  if(ERConstraint == 'selectedMain' & LRConstraint == 'selectedMain'){
    selectedConstraints <- read_csv(here::here(outPath, 'tables',
                                               'selected_constraints.csv')) %>% 
      filter(sensitivity == 'main', 
             subSetVar == 'fullSet', subSet == 'fullSet')
    ERConstraint <- selectedConstraints$ERConstraint[1]
    LRConstraint <- selectedConstraints$LRConstraint[1]
  }
  if(ERConstraint == 'selected' & LRConstraint == 'selected'){
    selectedConstraints <- read_csv(here::here(outPath, 'tables',
                                         'selected_constraints.csv')) %>% 
      filter(sensitivity == !!sensitivity, 
             subSetVar == !!subSetVar, subSet == !!subSet)
    ERConstraint <- selectedConstraints$ERConstraint[1]
    LRConstraint <- selectedConstraints$LRConstraint[1]
  }
  # 1c Create modelNames 
  modelName <- paste(sensitivity, subSetVar, subSet, 
                     ERConstraint, LRConstraint, sep = '_')  
  # 1d Read predictions of selected models
  # this line will generate the warning 'Missing column names filled in: 'X1' [1]'
  # this warning is just about some nonsense column, probably made by excel. 
  # since this warning shows up a lot, and it is annoying and uninformative 
  # I suppress it here. 
  suppressWarnings(
  est.table <- read_csv(here::here(outPath, 'estimates',
                                   paste0(indCumul, '_', modelName, '.csv'))) %>% 
    dplyr::select(refT, counterfactual_temp, label, contains('rr')) %>% 
    mutate(sensitivity = !!sensitivity, 
           subSetVar = !!subSetVar, subSet = !!subSet, indCumul = !!indCumul) 
  )
  # 1e Restrict effect estimates to the exposure range of interest
  if(expRange != 'all'){
    # 1e.i Break up the exposure range into lower and upper percentiles
    expRangeVec <- str_split_fixed(expRange, '_', 2)[1,]
    # 1e.ii Subset the TempObs dataset (we need it to determine value of percentile)
    if(subSetVar == 'season' | subSetVar == 'sutter_county'){
      tempObs.sub <- tempObs %>% 
        rename(subSetVar = !!subSetVar) %>%
        filter(subSetVar == subSet)
    } else{tempObs.sub <- tempObs}
    # 1e.iii Determine the values of the lower and upper temp percentile 
    temp.lowerLimSub <- quantile(tempObs.sub$temp, as.numeric(expRangeVec[1])/100, type = 1)
    temp.upperLimSub <- quantile(tempObs.sub$temp, as.numeric(expRangeVec[2])/100, type = 1)
    # 1e.iv Restrict the est table
    est.table <- est.table %>% 
      filter(counterfactual_temp > temp.lowerLimSub & 
               counterfactual_temp < temp.upperLimSub)
  }
  
  # 1f Return object 
  return(est.table)
  
}

