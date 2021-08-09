# Plot Lag-Response Curve for a Given Exposure Contrast
# Present Results
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# 0: Preparation 
# 1: Create Plotting Function 
  # 1A: Read Model Predictions
  # 1B: Make Plot
# 1: Create Plots

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('ran_g_01')){
  source(here::here('scripts', 'g_01_set_plottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plotERCurvePerLag <- function(sensitivity, subSetVar, subSet,
                                     ERConstraint, LRConstraint, expRange, examineThreshold = ''){
   #sensitivity <- 'explor' ; subSetVar <- 'dow'; subSet <- 'wk' 
  #ERConstraint <- 'selected'; LRConstraint <- 'selected';expRange <- '0_100'
  # examineThreshold <- ''
  
  # 1b Determine number of lags 
  if(str_detect(sensitivity, 'DayLag')){
    numLag <- as.numeric(str_remove_all(sensitivity, '[A-z]'))
  }else {numLag <- 14}
  
  ####********************************
  #### 1A: Read Model Predictions ####
  ####********************************
  
  # 1A.a Read Table 
  est.table <- readEstimates(sensitivity, subSetVar, subSet, 
                                      ERConstraint, LRConstraint, 'EstInd')
  
  # 1A.b Process Table
  est.table <- est.table %>% 
    dplyr::select(-'sensitivity', -subSetVar, -subSet, -indCumul, -label) %>%
    gather('lagName', 'estimate', -counterfactual_temp) %>% 
    mutate(var_name = str_sub(lagName, 1, 3), lagNum = as.numeric(str_sub(lagName, 11))) %>%  
    dplyr::select(-lagName) %>%
    spread(var_name, estimate) %>% 
    mutate(fit_pc = convertToPercent(fit), 
           lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)) 
  
  # 1A.c Determine temperature values used in all plots
  # the principle is that the we the x-axis will cover the whole range of observed 
  # temperatures, 
  # but the actual meat of the plot will only cover the relevant range for that subset
  # aka the values included in the histogram and the data
  # 1A.c.i Parse the upper and lower percentiles
  expRangeVec <- str_split_fixed(expRange, '_', 2)[1,]
  # 1A.c.ii Calculate values
  temp.per05 <- quantile(tempObs$temp, 0.05, type = 1)
  temp.per95 <- quantile(tempObs$temp, 0.95, type = 1)
  temp.lowerLim <- quantile(tempObs$temp, as.numeric(expRangeVec[1])/100, type = 1)
  temp.upperLim <- quantile(tempObs$temp, as.numeric(expRangeVec[2])/100, type = 1)
  
  # 1A.d Determine temperature values for the histogram specific for this subset
  # 1A.d.i Subset the temperature observations
  if(subSetVar == 'season' | subSetVar == 'sutter_county'){
    tempObs.sub <- tempObs %>% 
      rename(subSetVar = !!subSetVar) %>%
      filter(subSetVar == subSet)
  }else{tempObs.sub <- tempObs}
  # 1A.d.ii Calculate the upper and lower temperature values
 # temp.lowerLimSub <- quantile(tempObs.sub$temp, as.numeric(expRangeVec[1])/100, type = 1)
  #temp.upperLimSub <- quantile(tempObs.sub$temp, as.numeric(expRangeVec[2])/100, type = 1)
  # 1A.d.iii Curate temperature table
  #tempObs.sub <- tempObs %>% 
   # filter(temp > temp.lowerLimSub & 
    #         temp < temp.upperLimSub)
  
  # 1A.e Set range for y-axis
  if(expRange == '0_100'){est.min <- -20; est.max <- 10}
  if(expRange == '05_95'){est.min <- -10; est.max <- 15}
  # Identify highest and lowest estimates 
  #est.min <- min(est.table$lci_pc)
  #est.max <- max(est.table$uci_pc)
  
  ####*******************
  #### 1B: Make Plot ####
  ####*******************
  
  # 1B.a Make the histogram
  hist.plot <- ggplot(tempObs.sub) + 
    geom_histogram(aes(temp), binwidth =1) + 
    geom_vline(xintercept=temp.per05, color ='grey', linetype='dashed') +
    geom_vline(xintercept=temp.per95, color ='grey', linetype='dashed') +
    xlim(c(temp.lowerLim, temp.upperLim)) + 
    labs(x = expression(Daily~Temperature~(degree*C)), 
         y = 'Count') + 
    tema
  
  # 1B.b Set up colors 
  LC.l <- topo.colors(numLag)
  
  # 1B.c Set up which vertical lines to include 
  vertList <- c(temp.per05, temp.per95) 
  
  if(examineThreshold == 'examineThreshold'){ vertList <- c(vertList, 11:19)} 
    
  # 1B.c Create modelName 
  modelName <- paste(sensitivity, subSetVar, subSet, 
                     ERConstraint, LRConstraint, sep = '_')
  
  # 1B.d Start the PDF
     pdf(here::here(outPath, 'plots', 
                    paste0('g03_allLags_', modelName, '_', expRange, examineThreshold, '.pdf')))
     
  # 1B.e Create the lag-specific exposure-response plot for each lag
     # 1B.e.i Initialize loop
     for(i in 0:(numLag-1)){

       # 1B.e.iii Create Plot
       ER.plot <- est.table %>% 
         filter(lagNum == i) %>%
         ggplot(aes(counterfactual_temp)) + 
         geom_hline(yintercept=0, color ='grey' ) + 
         geom_vline(xintercept=vertList, color ='grey', linetype='dashed') +
         geom_vline(xintercept=mean(tempObs$temp), color ='orange') +
         geom_ribbon(aes(ymin= lci_pc,  ymax = uci_pc), fill = LC.l[i +1],  alpha  = 0.3) +
         geom_line(aes(y = fit_pc),  color = LC.l[i + 1])+ 
         labs(x = ' ', y = paste0('Change in \n UTI Rate (%)')) + 
         ggtitle(paste0( 'Exposure-Response \nfor ', i, ' Days Following Exposure'))+
        # coord_cartesian(ylim=c(-10, 10), expand = TRUE,
                         #default = FALSE, clip = 'off') + 
         xlim(c(temp.lowerLim, temp.upperLim)) + 
         scale_y_continuous(breaks = seq(est.min, est.max, by = 2), 
                            limits = c(est.min, est.max)) +
         annotate('text', x = (2+temp.per05), y = 1, label = '5th %tile', size = 4) + 
         annotate('text', x = (2+temp.per95), y = 1, label = '95th %tile', size = 4) + 
           annotate('text', x = (2+mean(tempObs$temp)), y = -1, label = 'Mean \n (Reference)', size = 4) + 
         tema
       # 1B.e.iv Output the two plots in one column
       print(cowplot::plot_grid(ER.plot, hist.plot, 
                                ncol=1, align = 'v', rel_heights = c(2, 1)))
     }
     # 1B.f Once the loop is complete, finish the pdf
     dev.off()
     
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Examine Threshold 
plotERCurvePerLag('main', 'fullSet', 'fullSet', 'selected', 'selected', '05_95', 'examineThreshold')

# 2b Examine main model 
plotERCurvePerLag('main', 'fullSet', 'fullSet', 'selected', 'selected', '05_95')
plotERCurvePerLag('main', 'fullSet', 'fullSet', 'selected', 'selected', '0_100')

plotERCurvePerLag('main', 'sex', 'f', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'sex', 'f', 'selected', 'selected', '0_100')

# cachement area
plotERCurvePerLag('main', 'sutter_county', 'sutter', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'sutter_county', 'sutter', 'selected', 'selected', '0_100')
plotERCurvePerLag('main', 'sutter_county', 'not_sutter', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'sutter_county', 'not_sutter', 'selected', 'selected', '0_100')

# Season 
plotERCurvePerLag('main', 'season', 'win', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'season', 'win', 'selected', 'selected', '0_100')
plotERCurvePerLag('main', 'season', 'spr', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'season', 'spr', 'selected', 'selected', '0_100')
plotERCurvePerLag('main', 'season', 'sum', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'season', 'sum', 'selected', 'selected', '0_100')
plotERCurvePerLag('main', 'season', 'fal', 'selected', 'selected', '05_95')
#plotERCurvePerLag('main', 'season', 'fal', 'selected', 'selected', '0_100')

# Females Only
# cachement area
plotERCurvePerLag('onlyF', 'sutter_county', 'sutter', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'sutter_county', 'sutter', 'selected', 'selected', '0_100')
plotERCurvePerLag('onlyF', 'sutter_county', 'not_sutter', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'sutter_county', 'not_sutter', 'selected', 'selected', '0_100')

# Season 
plotERCurvePerLag('onlyF', 'season', 'win', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'season', 'win', 'selected', 'selected', '0_100')
plotERCurvePerLag('onlyF', 'season', 'spr', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'season', 'spr', 'selected', 'selected', '0_100')
plotERCurvePerLag('onlyF', 'season', 'sum', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'season', 'sum', 'selected', 'selected', '0_100')
plotERCurvePerLag('onlyF', 'season', 'fal', 'selected', 'selected', '05_95')
#plotERCurvePerLag('onlyF', 'season', 'fal', 'selected', 'selected', '0_100')



# wknd 
plotERCurvePerLag('explor', 'dow', 'wk', 'selected', 'selected', '05_95')
plotERCurvePerLag('explor', 'dow', 'wknd', 'selected', 'selected', '05_95')
