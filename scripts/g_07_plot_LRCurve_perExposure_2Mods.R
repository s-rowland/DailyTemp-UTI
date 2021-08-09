# Plot Lag-Response Curve for a Given Exposure Contrast
# Present Results
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Plotting Function 
# 1A: Wrangle Model Independent Estimates
# 1B: Make Plot
# 2: Create Plots

####********************
#### 0: Preparation #### 
####********************

# Na: Smooth plot? 
# In our data setup, time is not smooth, but a step function - either you are in 
# Day 1 or Day 2, there is no such thing in our data as Day 1.5. 
# Day 1.5 would roughly correspond to exposure over the period of noon of Day 1 
# to noon of Day 2. 
# Since the crossbasis uses smooth terms like splines, we can actually estimate 
# the coefficients for exposure on Day 1.5. 
# Some researchers prefer to show point estimates for the individual lags, 
# others prefer to show smooth curves across all of the lags. 
# I don't have a strong preference; there is a tradeoff 
# the individual point estimates best represents what we measured, 
# but the smooth curve better illustrates the constraint we applied. 
# It also depends on the number of lags you are showing. 

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('ran_g_01')){
  source(here::here('scripts', 'g_01_set_PlottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_LRCurve_perExposure_2Mods <- function(
  sensitivityA, subSetVarA, subSetA, ERConstraintA, LRConstraintA,
  sensitivityB, subSetVarB, subSetB, ERConstraintB, LRConstraintB,
  contrast, curve){
  #sensitivityA <- 'Main' ; subSetVarA <- 'fullSet'; subSetA <- 'fullSet' 
  #ERConstraintA <- 'selected'; LRConstraintA <- 'selected'
  #sensitivityB <- '14DayLag' ; subSetVarB <- 'fullSet'; subSetB <- 'fullSet' 
  #ERConstraintB <- 'selected'; LRConstraintB <- 'selected';
  #Curve = 'Point'; Contrast <- '0595'
  
  
  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************
  
  # 1A.a Read model estimates
  # 2A.a Read Table 
  est.table <- bind_rows(readEstimates(sensitivityA, subSetVarA, subSetA, 
                                        ERConstraintA, LRConstraintA, 'EstInd'), 
                         readEstimates(sensitivityB, subSetVarB, subSetB, 
                                        ERConstraintB, LRConstraintB, 'EstInd'))
  
  # 1A.b Keep only relevant exposure contrast
  # 1A.b.i Determine the relevant Labels
  if(contrast == '05_95'){labelSet = c('per05', 'per95')}
  # 1A.b.ii Isolate the relevant exposure constrats
  est.table <- est.table %>% 
    filter(label %in% labelSet)
  
  # 1A.c Determine type of contrast 
  if(sensitivityA != sensitivityB){
    modelComparison <- paste0(subSetVarA, '_', subSetA, '_', 
                              ERConstraintA, '_', LRConstraintA, '_',
                              'sensitivity', '_', sensitivityA, '_Vs_', sensitivityB)
    est.table <- est.table %>% 
      mutate(mod_comp = sensitivity)
  }else if(subSetA != subSetB){
    modelComparison <- paste0(sensitivityA, '_', ERConstraintA, '_', LRConstraintA, '_',
                              'subSet', '_', subSetA, '_Vs_', subSetB)
    est.table <- est.table %>% 
      mutate(mod_comp = subSet)
  }else if(ERConstraintA != ERConstraintB){
    modelComparison <- paste0(sensitivityA, '_', subSetVarA, '_', subSetA, '_',
                              'ERConstraint', '_', ERConstraintA, '_Vs_', ERConstraintB)
    est.table <- est.table %>% 
      mutate(mod_comp = ERConstraint)
  }else if(LRConstraintA != LRConstraintB){
    modelComparison <- paste0(sensitivityA, '_', subSetVarA, '_', subSetA, '_',
                              'LRConstraint', '_', LRConstraintA, '_Vs_', LRConstraintB)
    est.table <- est.table %>% 
      mutate(mod_comp = LRConstraint)
  }
  
  # 1A.c Wrangle estimates 
  est.table <- est.table %>% 
    dplyr::select(-sensitivity, -subSetVar, -subSet, -indCumul, -label) %>%
    gather('lag_name', 'estimate', -counterfactual_temp, -mod_comp) %>% 
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%  
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>% 
    mutate(fit_pc = convertToPercent(fit), 
           lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)) 
  
  # 1A.d Convert counterfactual_temp to character so that you can use discrete color scale
  est.table <- est.table %>% 
    mutate(counterfactual_temp = as.character(round(counterfactual_temp, 2))) 
  est.table <- est.table %>% 
    mutate(counterfactual_temp =
             factor(counterfactual_temp,
                    levels = c(min(as.numeric(est.table$counterfactual_temp)), 
                               max(as.numeric(est.table$counterfactual_temp)))))
  
  # 1A.e Isolate to whole-number lags 
  # only for the point estimate plot 
  # 1A.e.i Determine the number of lags
  if(str_detect(paste0(sensitivityA, sensitivityB), 'DayLag')) {
    numLag <- max(c(as.numeric(str_remove_all(sensitivityA, '[A-z]')), 
                    as.numeric(str_remove_all(sensitivityB, '[A-z]')) ), 
                  na.rm = TRUE)  
  }else{numLag <- 7}
  # 1A.e.ii restrict to whole-number lags
  if(curve == 'Point'){
    est.table <- est.table %>% filter(lag_day %in% 0:numLag)}
  
  # set range of y-axis
  if(contrast == '0_100'){est.min <- -20; est.max <- 10}
  if(contrast == '05_95'){est.min <- -7; est.max <- 6}
  
  ####*******************
  #### 1B: Make Plot ####
  ####******************* 
  
  # 1B.a Determine type of curve 
  if(curve == 'Smooth'){
    ggLR1 <- geom_ribbon(aes(ymin= lci_pc,  ymax = uci_pc, fill = mod_comp), 
                 alpha  = 0.35, col=NA)
    ggLR2 <- geom_line(aes(y = fit_pc, col = mod_comp), size = 2)
  }
  if(curve == 'Point'){
    ggLR1 <- geom_point(aes(y = fit_pc, fill= mod_comp, color = mod_comp, shape = mod_comp), size = 3)
    ggLR2 <-  geom_errorbar(aes(ymin= lci_pc,  ymax = uci_pc, color = mod_comp), size = 0.75)
  }
  
  # 1B.a Create plot 
  TP.a <- est.table %>%
    ggplot(aes(lag_day)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    ggLR1 + ggLR2 +
    facet_grid(.~counterfactual_temp) +
    scale_fill_manual(values = colorArray$modContrast) + 
    scale_color_manual(values = colorArray$modContrast) + 
    labs(y = paste0('Change in UTI Rate (%)'), 
         x = paste0('Days Since Exposure')) + 
    scale_x_continuous(breaks = c(0, 3, 7, 10, 13)) +
    scale_y_continuous(breaks = seq(est.min, est.max, by = 2), 
                       limits = c(est.min, est.max)) +
    tema + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank())  +
    theme(panel.background = element_rect(fill= NA, color = 'black')) +
    theme(legend.title=element_text(size=20),
          legend.text=element_text(size=16),
          legend.position = c(0.8, 0.84), 
          legend.key.size = unit(0.4, 'cm')) + 
    theme(axis.title.y = element_text(size = 20), 
          axis.text.x = element_text(size = 16), 
          axis.text.y = element_text(size = 16)) 
  
  # 1B.b Print plots
  png(here::here(outPath, 'plots',
                 paste0('g07_LR_', modelComparison, '_', contrast,'_', curve, '.png')), 
      width = ww.fig*2, height = hh.fig*0.75, res = rr.fig*1)
  print(tag_facet(TP.a, 
                  tag_pool = paste(distinct(est.table,counterfactual_temp)$counterfactual_temp, ' deg C'), 
                  x = -0.5, y = 0.75*max(est.table$uci_pc, na.rm = TRUE)))
  dev.off()
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Main Models
plot_LRCurve_perExposure_2Mods('main', 'fullSet', 'fullSet', 'selected', 'selected',
                               'main', 'sex', 'f', 'selected', 'selected',
                               '05_95', 'Smooth')
plot_LRCurve_perExposure_2Mods('main', 'fullSet', 'fullSet', 'selected', 'selected',
                               'main', 'sex', 'f', 'selected', 'selected',
                               '05_95', 'Point')

plot_LRCurve_perExposure_2Mods('main', 'sutter_county', 'sutter', 'selected', 'selected',
                               'main', 'sutter_county', 'not_sutter', 'selected', 'selected',
                               '05_95', 'Smooth')

plot_LRCurve_perExposure_2Mods('explor', 'dow', 'wk', 'selected', 'selected', 
                               'explor', 'dow', 'wknd', 'selected', 'selected', 
                               '05_95', 'Point')
