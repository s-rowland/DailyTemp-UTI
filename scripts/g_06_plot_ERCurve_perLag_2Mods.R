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
  sensitivityA, subSetVarA, subSetA, ERConstraintA, LRConstraintA,
  sensitivityB, subSetVarB, subSetB, ERConstraintB, LRConstraintB, expRange){
  #sensitivityA <- 'main' ; subSetVarA <- 'fullSet'; subSetA <- 'fullSet' 
  #ERConstraintA <- 'selected'; LRConstraintA <- 'selected'
  #sensitivityB <- 'main' ; subSetVarB <- 'sex'; subSetB <- 'f' 
  #ERConstraintB <- 'selected'; LRConstraintB <- 'selected';expRange <- '0_100'
  
  
  # 2b Determine number of lags 
  if(str_detect(paste0(sensitivityA, sensitivityB), 'DayLag')) {
    numLag <- max(c(as.numeric(str_remove_all(sensitivityA, '[A-z]')), 
                    as.numeric(str_remove_all(sensitivityB, '[A-z]')) ), 
                  na.rm = TRUE)  
  }else{numLag <- 14}
  
  # 2c Determine exposure vector
  expRangeVec <- str_split_fixed(expRange, '_', 2)[1,]
  
  ####********************************
  #### 2A: Read Model Predictions ####
  ####********************************
  
  # 2A.a Read Table 
  est.table <- bind_rows(readEstimates(sensitivityA, subSetVarA, subSetA, 
                                      ERConstraintA, LRConstraintA, 'EstInd', expRange), 
                         readEstimates(sensitivityB, subSetVarB, subSetB, 
                                        ERConstraintB, LRConstraintB, 'EstInd', expRange))
                         
  # 2A.c Determine type of contrast 
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
  
  # wrangel estimate table
  est.table <- est.table %>% 
    dplyr::select(-'sensitivity', -subSetVar, -subSet, -indCumul, -label) %>%
    gather('lag_name', 'estimate', -counterfactual_temp, -mod_comp) %>% 
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%  
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>% 
    mutate(fit_pc = convertToPercent(fit), 
           lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)) 
  
  # 1A.c Determine temperature values used in all plots
  # the principle is that the we the x-axis will cover the whole range of observed 
  # temperatures, 
  # but the actual meat of the plot will only cover the relevant range for that subset
  # 1A.c.i Parse the upper and lower percentiles
  expRangeVec <- str_split_fixed(expRange, '_', 2)[1,]
  # 1A.c.ii Calculate values
  temp.per05 <- quantile(tempObs$temp, 0.05, type = 1)
  temp.per95 <- quantile(tempObs$temp, 0.95, type = 1)
  temp.lowerLim <- quantile(tempObs$temp, as.numeric(expRangeVec[1])/100, type = 1)
  temp.upperLim <- quantile(tempObs$temp, as.numeric(expRangeVec[2])/100, type = 1)
  
  # the principle is that the we the x-axis will cover the whole range of observed 
  # temperatures, 
  # but the actual meat of the plot will only cover the relevant range for that subset
  # 1A.d Curate temperature table
  
  # 1A.e Curate estimate table

  
  # 1A.f Identify highest and lowest estimates 
  if(expRange == '0_100'){est.min <- -20; est.max <- 10}
  if(expRange == '05_95'){est.min <- -7; est.max <- 6}
  
  #est.min <- min(est.table$lci_pc)
  #est.max <- max(est.table$uci_pc)
  
  ####*******************
  #### 2B: Make Plot ####
  ####*******************
  
  # 2B.a Make the histogram
# No histogram! 
  
  # 2B.b Start the PDF
     pdf(here::here(outPath, 'plots', 
                    paste0('g06_allLags_', modelComparison, '_', expRange, '.pdf')))
     
  # 2B.c Create the lag-specific exposure-response plot for each lag
     # 2B.c.i Initialize loop
     for(i in 0:(numLag-1)){
       # 2B.c.iii Create Plot
       ER.plot <- est.table %>% 
         filter(lag_day == i) %>%
         ggplot(aes(counterfactual_temp)) + 
         geom_hline(yintercept=0, color ='grey' ) + 
         geom_vline(xintercept=temp.per05, color ='grey', linetype='dashed') +
         geom_vline(xintercept=temp.per95, color ='grey', linetype='dashed') +
         geom_ribbon(aes(ymin= lci_pc,  ymax = uci_pc, fill = mod_comp),  alpha  = 0.2)+
         geom_line(aes(y = fit_pc, color = mod_comp))+ 
         labs(x = ' ', y = paste0('Change in \n UTI Rate (%)')) + 
         scale_fill_manual(values = colorArray$modContrast) + 
         scale_color_manual(values = colorArray$modContrast) + 
         ggtitle(paste0( 'Exposure-Response \nfor ', i, ' Days Following Exposure'))+
        # coord_cartesian(ylim=c(-10, 10), expand = TRUE,
                         #default = FALSE, clip = 'off') + 
         #scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
         xlim(c(temp.lowerLim, temp.upperLim)) + 
         scale_y_continuous(breaks = seq(est.min, est.max, by = 2), 
                            limits = c(est.min, est.max)) +
         annotate('text', x = (2+temp.per05), y = 1, label = '5th %tile', size = 4) + 
         annotate('text', x = (2+temp.per95), y = 1, label = '95th %tile', size = 4) + 
         annotate('text', x = (2+mean(tempObs$temp)), y = -1, label = 'Mean \n (Reference)', size = 4) + 
         tema
       # 2B.c.iv Output the two plots in one column
       print(cowplot::plot_grid(ER.plot, #hist.plot, 
                                ncol=1, align = 'v', rel_heights = c(1, 1)))
     }
     # 2B.d Once the loop is complete, finish the pdf
     dev.off()
     
}

####*********************
#### 3: Create Plots ####
####*********************

# 3a Main Models 
plot_ERCurve_perLag_2Mods('main', 'fullSet', 'fullSet', 'selected', 'selected', 
                        'main', 'sex','f', 'selected', 'selected', 
                        '0_100')
plot_ERCurve_perLag_2Mods('main', 'fullSet', 'fullSet', 'selected', 'selected', 
                          'main', 'sex','f', 'selected', 'selected', 
                          '05_95')

plot_ERCurve_perLag_2Mods('main', 'sutter_county', 'sutter', 'selected', 'selected', 
                          'main', 'sutter_county','not_sutter', 'selected', 'selected', 
                          '0_100')
plot_ERCurve_perLag_2Mods('main', 'sutter_county', 'sutter', 'selected', 'selected', 
                          'main', 'sutter_county','not_sutter', 'selected', 'selected', 
                          '05_95')


plot_ERCurve_perLag_2Mods('explor', 'dow', 'wk', 'selected', 'selected', 
                          'explor', 'dow', 'wknd', 'selected', 'selected', 
                          '05_95')
