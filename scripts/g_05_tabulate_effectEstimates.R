# Create eTable2: effect estimates of sensitivity analyses 
# HourlyTemp-Stroke Analysis
# Temperature-CVD-NYS Project 
# Sebastian T. Rowland 
# Updated Jan 26, 2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Table

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts','0_01_setUp_for_Analysis.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('ran_g_01')){
  source(here::here('scripts', 'g_01_set_PlottingObjects.R'))
}

####*********************
#### 1: Create Table ####
####*********************

# 1a Declare the exposure contrast of interest
labelSet = c('per05', 'per95')

# 1b Setup table of models to tabulate
SensitivityList <- rep('Main', 2)
SubSetVarList <- rep('FullSet', 2)
SubSetList <- rep('FullSet', 2)
ERConstraintList <- rep('Selected', 2)
LRConstraintList <- rep('Selected', 2)
IndCumulList <- c('EstInd', 'EstCumul')

# 1c Readin estimates 
est.list <- purrr::pmap(list(SensitivityList, SubSetVarList, SubSetList, 
                             ERConstraintList, LRConstraintList, IndCumulList), 
                        read_estimates)
est.table <- bind_rows(est.list)

# 1d Keep only the exposure contrasts of interest 
est.table <- est.table %>% 
  filter(Label %in% labelSet)

# 1e Round CounterfactualTemp so that it is presentable
est.table <- est.table %>% 
  mutate(CounterfactualTemp = round(CounterfactualTemp, 2))

# 1f As usual, wrangle the estimates
est.table  <- est.table %>% 
  dplyr::select(-Sensitivity, -SubSetVar, -SubSet, -Label) %>%
  gather('LagName', 'Estimate', -CounterfactualTemp, -IndCumul) %>% 
  mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
  dplyr::select(-LagName) %>%
  filter(!str_detect(Lag, '\\.')) %>%
  spread(VarName, Estimate) %>% 
  mutate(Lag = as.character(str_pad(Lag, 2, 'left', '0')))%>% 
  mutate(fit.pc = convert_to_percent(fit), 
         lci.pc = convert_to_percent(lci), 
         uci.pc = convert_to_percent(uci))

# 1g Save one version of table
est.table %>% 
  write_csv(here::here(outPath, 'tables', 'EE_FullSetAnalysis.csv'))

# 1h Put table in a tidy format, with 3 columns
est.table <- est.table %>% 
  mutate(EE = paste0(format(round(fit.pc, 1), nsmall = 1), ' (', 
                     format(round(lci.pc, 1), nsmall = 1), ', ', 
                     format(round(uci.pc, 1), nsmall = 1), ')')) 

# 1i Spread out effect estimates into 4 columns 
est.table <- est.table %>% 
  mutate(EXPName = paste0(IndCumul, '_', CounterfactualTemp)) %>%
  dplyr::select(EXPName, Lag,  EE) %>% 
  spread(EXPName, EE)  

# 1j Save table
est.table %>% 
  write_csv(here::here(outPath, 'tables',
                       'EE_FullSetAnalysis_pretty.csv'))
