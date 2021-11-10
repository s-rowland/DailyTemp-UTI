# Perform Grid Search to Identify set of Constraints that Yields Lowest QAIC
# Functions
# Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Get QAIC of Candidate Models
# 3: Calculate QAIC Weights
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
  # sensitivity <- 'main';  subSetVar <- 'fullSet'; subSet <- 'fullSet'
  
  # 1a Add progress bar
  # Silenced for now as progress bar is not behaving well. 
  #pb$tick()
  
  ####************************************
  #### 2: Get QAIC of Candidate Models ####
  ####************************************
  
  # 2a Initialize loop over grid cells
  # we do a loop here rather than parallelize because 
  # fitting the model is memory-intensive, and R would crash.
  # Might be able to do parallel analysis for UTI dataset. 
  for(i in 1:nrow(candidateConstraintsGrid)){
    # i <- 1
    # 2b Fit model with candidate constraints
    analyzeTempDLNM(sensitivity = sensitivity, 
                    subSetVar = subSetVar, 
                    subSet = subSet, 
                    ERConstraint = candidateConstraintsGrid$ERConstraint[i], 
                    LRConstraint = candidateConstraintsGrid$LRConstraint[i], 
                    saveModel = 'saveQAIC')
  }
  
  ####*******************************
  #### 3: Calculate QAIC Weights ####
  ####*******************************
  
  # 3a Readin table of model QAICs  
  qaic.table0 <- read_csv(here::here(outPath, 'tables', 'model_QAIC.csv'), 
                         col_types = 'cccccddT')
  
  # 3c Keep only the QAIC of models with the same parameters (aside from constraints)
  qaic.table <- qaic.table0 %>% 
    filter(sensitivity == !!sensitivity & 
           subSetVar == !!subSetVar & subSet == !!subSet)
  
  # 3d Find minQAIC
  QAIC.min <- min(qaic.table$QAIC)
  
  # 3e Calculate deltaQAIC 
  qaic.table <- qaic.table  %>% 
      mutate(deltaQAIC = as.numeric(QAIC) - as.numeric(QAIC.min))
  
  # 3f Calculate weight denominator
  denom.qaic <- sum(exp(-0.5 * qaic.table$deltaQAIC)) 
    
  # 3g Calculate QAIC weights 
  qaic.table$AkaikeWeight <- exp(-0.5*qaic.table$deltaQAIC) / denom.qaic
    
  # 3h Put weights in pretty format
  qaic.table$AkaikeWeight <- round(100*qaic.table$AkaikeWeight, 1)
    
  # 3i Add to previous QAIC table
  qaicWeights.table <- qaic.table %>% 
      dplyr::select(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint, 
                    QAIC, AkaikeWeight, run_date) %>%
     bind_rows(read_csv(here::here(outPath, 'tables', 'model_QAIC.csv'), 
                        col_types = 'cccccddT'))
  
  # 3j Keep only the most recent versions of each model and save 
  qaicWeights.table %>% 
      group_by(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint) %>% 
      arrange(desc(run_date)) %>% 
      slice(0:1) %>% 
      filter(!is.na(sensitivity)) %>%
      write_csv(here::here(outPath, 'tables', 'model_QAIC.csv'))
    
  ####*********************
  #### 4: Select Model ####
  ####*********************
  
  # 4a Identify selected model 
  selectedModel <- qaic.table %>% 
      filter(QAIC == QAIC.min) %>% 
      dplyr::select(sensitivity, subSetVar, subSet, ERConstraint, LRConstraint, 
                    run_date) 
  
  # 4b Add to previous selected models 
  selectedModels.table <- selectedModel %>% 
    bind_rows(read_csv(here::here(outPath, 'tables', 'selected_constraints.csv'),  
                       col_types = 'cccccT')) 
  
  # 4c Keep only the QAIC of the most recent versions of each model and save 
  selectedModels.table %>%
    group_by(sensitivity, subSetVar, subSet) %>% 
    arrange(desc(run_date)) %>% 
    slice(0:1) %>% 
    filter(!is.na(sensitivity)) %>%
    write_csv(here::here(outPath, 'tables', 'selected_constraints.csv'))
}