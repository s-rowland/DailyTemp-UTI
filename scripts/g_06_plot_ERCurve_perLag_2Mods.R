# Plot Lag-Response Curve for a Given Exposure Contrast
# Present Results
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Set Up Plotting Objects 
# 2: Create Plotting Function 
  # 2A: Read Model Predictions
  # 2B: Make Plot
# 3: Create Plots

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

####********************************
#### 1: Set Up Plotting Objects #### 
####********************************


####*********************************
#### 2: Create Plotting Function ####
####*********************************

# 2a Name function 
plot_ERCurve_perLag_2Mods <- function(
  SensitivityA, SubSetVarA, SubSetA, ERConstraintA, LRConstraintA,
  SensitivityB, SubSetVarB, SubSetB, ERConstraintB, LRConstraintB, ExpRange){
  #SensitivityA <- 'Main' ; SubSetVarA <- 'FullSet'; SubSetA <- 'FullSet' 
  #ERConstraintA <- 'Selected'; LRConstraintA <- 'Selected'
  #SensitivityB <- '21DayLag' ; SubSetVarB <- 'FullSet'; SubSetB <- 'FullSet' 
  #ERConstraintB <- 'Selected'; LRConstraintB <- 'Selected';ExpRange <- '0_100'
  
  
  # 2b Determine number of lags 
  if(str_detect(paste0(SensitivityA, SensitivityB), 'DayLag')) {
    numLag <- max(c(as.numeric(str_remove_all(SensitivityA, '[A-z]')), 
                    as.numeric(str_remove_all(SensitivityB, '[A-z]')) ), 
                  na.rm = TRUE)  
  }else{numLag <- 7}
  
  ####********************************
  #### 2A: Read Model Predictions ####
  ####********************************
  
  # 2A.a Read Table 
  est.table <- bind_rows(read_estimates(SensitivityA, SubSetVarA, SubSetA, 
                                      ERConstraintA, LRConstraintA, 'EstInd'), 
                         read_estimates(SensitivityB, SubSetVarB, SubSetB, 
                                        ERConstraintB, LRConstraintB, 'EstInd'))
                         
  # 2A.b Determine percentages
  Temp.per05 <- quantile(temper$tmean, 0.05, type = 1)
  Temp.per95 <- quantile(temper$tmean, 0.95, type = 1)
  
  ExpRangeVec <- str_split_fixed(ExpRange, '_', 2)[1,]
  Temp.perMin <- quantile(temper$tmean, as.numeric(ExpRangeVec[1])/100, type = 1)
  Temp.perMax <- quantile(temper$tmean, as.numeric(ExpRangeVec[2])/100, type = 1)
  
  # 2A.c Determine type of contrast 
  if(SensitivityA != SensitivityB){
    ModelComparison <- paste0(SubSetVarA, '_', SubSetA, '_', 
                              ERConstraintA, '_', LRConstraintA, '_',
                              'Sensitivity', '_', SensitivityA, '_Vs_', SensitivityB)
    est.table <- est.table %>% 
      mutate(ModComp = Sensitivity)
  }else if(SubSetA != SubSetB){
    ModelComparison <- paste0(SensitivityA, '_', ERConstraintA, '_', LRConstraintA, '_',
                              'SubSet', '_', SubSetA, '_Vs_', SubsetB)
    est.table <- est.table %>% 
      mutate(ModComp = SubSet)
  }else if(ERConstraintA != ERConstraintB){
    ModelComparison <- paste0(SensitivityA, '_', SubSetVarA, '_', SubSetA, '_',
                              'ERConstraint', '_', ERConstraintA, '_Vs_', ERConstraintB)
    est.table <- est.table %>% 
      mutate(ModComp = ERConstraint)
  }else if(LRConstraintA != LRConstraintB){
    ModelComparison <- paste0(SensitivityA, '_', SubSetVarA, '_', SubSetA, '_',
                              'LRConstraint', '_', LRConstraintA, '_Vs_', LRConstraintB)
    est.table <- est.table %>% 
      mutate(ModComp = LRConstraint)
  }
  
  ####*******************
  #### 2B: Make Plot ####
  ####*******************
  
  # 2B.a Make the histogram
  hist.plot <- ggplot(temper) + 
    geom_histogram(aes(tmean), binwidth =1) + 
    geom_vline(xintercept=Temp.per05, color ='grey', linetype='dashed') +
    geom_vline(xintercept=Temp.per95, color ='grey', linetype='dashed') +
    labs(x = expression(Daily~Temperature~(degree*C)), 
         y = 'Count') + 
    tema
  
  # 2B.b Start the PDF
     pdf(here::here(outPath, 'plots', 
                    paste0('g06_allLags_', ModelComparison, '_', ExpRange, '.pdf')))
     
  # 2B.c Create the lag-specific exposure-response plot for each lag
     # 2B.c.i Initialize loop
     for(i in 0:(numLag-1)){
       # 2B.c.ii Rename variables to general name
       name.fit <- paste0('fit.rr.lag', i)
       name.lci <- paste0('lci.rr.lag', i)
       name.uci <- paste0('uci.rr.lag', i)
       # 2B.c.iii Create Plot
       ER.plot <- est.table %>% 
         rename(fit.or = !!name.fit, 
                lci.or = !!name.lci, 
                uci.or = !!name.uci) %>% 
         mutate(fit.pc = convert_to_percent(fit.or), 
                lci.pc = convert_to_percent(lci.or),
                uci.pc = convert_to_percent(uci.or)) %>%
         ggplot(aes(CounterfactualTemp)) + 
         geom_hline(yintercept=0, color ='grey' ) + 
         geom_vline(xintercept=Temp.per05, color ='grey', linetype='dashed') +
         geom_vline(xintercept=Temp.per95, color ='grey', linetype='dashed') +
         geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc, fill = ModComp),  alpha  = 0.3)+
         geom_line(aes(y = fit.pc, color = ModComp))+ 
         labs(x = ' ', y = paste0('Change in \n UTI Rate (%)')) + 
         scale_fill_manual(values = ColorArray$ModContrast) + 
         scale_color_manual(values = ColorArray$ModContrast) + 
         ggtitle(paste0( 'Exposure-Response \nfor ', i, ' Days Following Exposure'))+
        # coord_cartesian(ylim=c(-10, 10), expand = TRUE,
                         #default = FALSE, clip = 'off') + 
         #scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
         xlim(c(Temp.perMin, Temp.perMax)) + 
         annotate('text', x = (2+Temp.per05), y = 1, label = '5th %tile', size = 4) + 
         annotate('text', x = (2+Temp.per95), y = 1, label = '95th %tile', size = 4) + 
         tema
       # 2B.c.iv Output the two plots in one column
       print(cowplot::plot_grid(ER.plot, hist.plot, 
                                ncol=1, align = 'v', rel_heights = c(2, 1)))
     }
     # 2B.d Once the loop is complete, finish the pdf
     dev.off()
     
}

####*********************
#### 3: Create Plots ####
####*********************

# 3a Main Models 
plot_ERCurve_perLag_2Mods('Main', 'FullSet', 'FullSet', 'Selected', 'Selected', 
                        '14DayLag', 'FullSet','FullSet', 'Selected', 'Selected', 
                        '0_100')
plot_ERCurve_perLag_2Mods('21DayLag', 'FullSet', 'FullSet', 'Selected', 'Selected', 
                        '14DayLag', 'FullSet','FullSet', 'Selected', 'Selected', 
                        '0_100')


plot_ERCurve_perLag_2Mods('21DayLag', 'FullSet', 'FullSet', '3dfevenknots', '3dfevenknots', 
                          '14DayLag', 'FullSet','FullSet', '3dfevenknots', '3dfevenknots',
                          '0_100')

plot_ERCurve_perLag_2Mods('21DayLag', 'FullSet', 'FullSet', '3dfevenknots', '4dflogknots',
                          '14DayLag', 'FullSet','FullSet', '3dfevenknots', '4dflogknots', 
                          '0_100')

plot_ERCurve_perLag_2Mods('21DayLag', 'FullSet', 'FullSet', '3dfevenknots', 'psp', 
                          '14DayLag', 'FullSet','FullSet', '3dfevenknots', 'psp', 
                          '0_100')