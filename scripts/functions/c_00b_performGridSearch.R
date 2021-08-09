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

performGridSearch <- function(candidateConstraintsGrid, sensitivity, subSetVar, subSet){
  # sensitivity <- 'Main';  subSetVar <- 'FullSet'; subSet <- 'FullSet'
  
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
    analyzeTempDLNM(sensitivity, subSetVar, subSet, 
                     candidateConstraintsGrid$ERConstraint[i], 
                     candidateConstraintsGrid$LRConstraint[i], 
                     'saveAIC')
  }
  
  ####******************************
  #### 3: Calculate AIC Weights ####
  ####******************************
  
  # 3a Readin table of model AICs  
  aic.table0 <- read_csv(here::here(outPath, 'tables', 'model_AIC.csv'), 
                         col_types = 'cccccddT')
  
  # 3c Keep only the AIC of models with the same parameters (aside from constraints)
  aic.table <- aic.table0 %>% 
    filter(sensitivity == !!sensitivity & 
           subSetVar == !!subSetVar & subSet == !!subSet)
  
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
      dplyr::select(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint, 
                    AIC, AkaikeWeight, run_date) %>%
     bind_rows(read_csv(here::here(outPath, 'tables', 'model_AIC.csv'), 
                        col_types = 'cccccddT'))
  
  # 3j Keep only the most recent versions of each model and save 
  aicWeights.table %>% 
      group_by(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint) %>% 
      arrange(desc(run_date)) %>% 
      slice(0:1) %>% 
      filter(!is.na(sensitivity)) %>%
      write_csv(here::here(outPath, 'tables', 'model_AIC.csv'))
    
  ####*********************
  #### 4: Select Model ####
  ####*********************
  
  # 4a Identify selected model 
  selectedModel <- aic.table %>% 
      filter(AIC == AIC.min) %>% 
      dplyr::select(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint, 
                    run_date) 
  
  # 4b Add to previous selected models 
  selectedModels.table <- selectedModel %>% 
    bind_rows(read_csv(here::here(outPath, 'tables', 'selected_constraints.csv'),  
                       col_types = 'cccccT')) 
  
  # 4c Keep only the AIC of the most recent versions of each model and save 
  selectedModels.table %>%
    group_by(sensitivity, subSetVar, subSet) %>% 
    arrange(desc(run_date)) %>% 
    slice(0:1) %>% 
    filter(!is.na(sensitivity)) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints.csv'))
}