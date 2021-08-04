# Perform Grid Search to Identify set of Constraints that Yields Lowest AIC
# Functions
# Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/18/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Get AIC of Candidate Models
# 3: Calculate AIC Weights
# 4: Select Model 

####**************
#### N: Notes #### 
####**************

# Na Description
# If in the whole analysis you only conduct one grid search, you don't really 
# need a function. 
# I have found that making a function for the grid search makes it much easier 
# for me to do the grid search for any subsequent analysis. 

####********************
#### 0: Preparation #### 
####********************

####***********************
#### 1: Begin Function ####
####***********************

perform_gridSearch <- function(candidateConstraintsGrid, Sensitivity, SubSetVar, SubSet){
  # Sensitivity <- 'Main';  SubSetVar <- 'FullSet'; SubSet <- 'FullSet'
  
  # 1a Add progress bar
  # Silenced for now as progress bar is not behaving well. 
  #pb$tick()
  
  ####************************************
  #### 2: Get AIC of Candidate Models ####
  ####************************************
  
  # 2a Initialize loop over grid cells
  # we do a loop here rather than parallelize because 
  # fitting the model is memory-intensive, and R would crash.
  # Might be able to do parallel analysis for UTI dataset. 
  for(i in 1:nrow(candidateConstraintsGrid)){
    # i <- 1
    # 2b Fit model with candidate constraints
    analyze_dlnmTemp(Sensitivity, SubSetVar, SubSet, 
                     candidateConstraintsGrid$ERConstraint[i], 
                     candidateConstraintsGrid$LRConstraint[i], 
                     'SaveAIC')
  }
  
  ####******************************
  #### 3: Calculate AIC Weights ####
  ####******************************
  
  # 3a Readin table of model AICs  
  aic.table0 <- read_csv(here::here(outPath, 'tables', 'Model_AIC.csv'), 
                         col_types = 'cccccddT')
  
  # 3c Keep only the AIC of models with the same parameters (aside from constraints)
  aic.table <- aic.table0 %>% 
    filter(Sensitivity == !!Sensitivity & 
           SubSetVar == !!SubSetVar & SubSetVar == !!SubSetVar)
  
  # 3d Find minAIC
  AIC.min <- min(aic.table$AIC)
  
  # 3e Calculate deltaAIC 
  aic.table <- aic.table  %>% 
      mutate(deltaAIC = as.numeric(AIC) - as.numeric(AIC.min))
  
  # 3f Calculate weight denominator
  denom.aic <- sum(exp(-0.5 * aic.table$deltaAIC)) 
    
  # 3g Calculate AIC weights 
  aic.table$AkaikeWeight <- exp(-0.5*aic.table$deltaAIC) / denom.aic
    
  # 3h Put weights in pretty format
  aic.table$AkaikeWeight <- round(100*aic.table$AkaikeWeight, 1)
    
  # 3i Add to previous AIC table
  aicWeights.table <- aic.table %>% 
      dplyr::select(Sensitivity, SubSetVar, SubSet, ERConstraint, LRConstraint, 
                    AIC, AkaikeWeight, RunDate) %>%
     bind_rows(read_csv(here::here(outPath, 'tables', 'Model_AIC.csv'), 
                        col_types = 'cccccddT'))
  
  # 3j Keep only the most recent versions of each model and save 
  aicWeights.table %>% 
      group_by(Sensitivity, SubSetVar, SubSet, ERConstraint, LRConstraint) %>% 
      arrange(desc(RunDate)) %>% 
      slice(0:1) %>% 
      filter(!is.na(Sensitivity)) %>%
      write_csv(here::here(outPath, 'tables', 'Model_AIC.csv'))
    
  ####*********************
  #### 4: Select Model ####
  ####*********************
  
  # 4a Identify selected model 
  SelectedModel <- aic.table %>% 
      filter(AIC == AIC.min) %>% 
      dplyr::select(Sensitivity, SubSetVar, SubSet, ERConstraint, LRConstraint, 
                    RunDate) 
  
  # 4b Add to previous selected models 
  SelectedModels.table <- SelectedModel %>% 
    bind_rows(read_csv(here::here(outPath, 'tables', 'SelectedModels.csv'),  
                       col_types = 'cccccT')) 
  
  # 4c Keep only the AIC of the most recent versions of each model and save 
  SelectedModels.table %>%
    group_by(Sensitivity, SubSetVar, SubSet) %>% 
    arrange(desc(RunDate)) %>% 
    slice(0:1) %>% 
    filter(!is.na(Sensitivity)) %>%
    write_csv(here::here(outPath, 'tables', 'SelectedModels.csv'))
}