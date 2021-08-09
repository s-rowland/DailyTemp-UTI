# Plot Lag-Response Curve for a Given Exposure contrast
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
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

# 0b Create the plotting objects, if you haven't already
if (!exists('ran_g_01')){
  source(here::here('scripts', 'G_01_set_plottingObjects.R'))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function 
plot_LRCurve_perExposure <- function(sensitivity, subSetVar, subSet, 
                          ERConstraint, LRConstraint, contrast){
  # sensitivity <- 'main' ; subSetVar <- 'fullSet'; subSet <- 'fullSet'
  # ERConstraint <- 'selected'; LRConstraint <- 'selected'
  # contrast <- '05_95'
  
  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************
  
  # 1A.a Read model estimates
  # 2A.a Read Table 
  est.table <- readEstimates(sensitivity, subSetVar, subSet, 
                              ERConstraint, LRConstraint, 'EstInd')
  
  # 1A.b Keep only relevant exposure contrast
  # 1A.b.i Determine the relevant Labels
  if(contrast == '05_95'){labelSet = c('per05', 'per95')}
  if(contrast == '05_25'){labelSet = c('per05', 'per25')}
  # 1A.b.ii Isolate the relevant exposure constrats
  est.table <- est.table %>% 
    filter(label %in% labelSet)
  
  # 1A.c Wrangle estimates 
  est.table <- est.table %>% 
    dplyr::select(-sensitivity, -subSetVar, -subSet, -indCumul, -label) %>%
    gather('lag_name', 'estimate', -counterfactual_temp) %>% 
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%  
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>% 
    mutate(fit_pc = convertToPercent(fit), 
           lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)) 
  
  # 1A.d Convert CounterfactualTemp to character so that you can use discrete color scale
  est.table <- est.table %>% 
    mutate(counterfactual_temp = as.character(round(counterfactual_temp, 2))) 
  est.table <- est.table %>% 
    mutate(counterfactual_temp =
             factor(counterfactual_temp,
                    levels = c(min(as.numeric(est.table$counterfactual_temp)), 
                               max(as.numeric(est.table$counterfactual_temp)))))
  # set range of y-axis
  if(contrast == '0_100'){est.min <- -20; est.max <- 10}
  if(contrast == '05_95'){est.min <- -7; est.max <- 6}
  
  ####*******************
  #### 1B: Make Plot ####
  ####******************* 
  
  # 1B.a Create plot 
  TP.a <- est.table %>%
    ggplot(aes(lag_day)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    geom_ribbon(aes(ymin= lci_pc,  ymax = uci_pc, fill = counterfactual_temp), 
                alpha  = 0.35, col=NA)+
    geom_line(aes(y = fit_pc, col = counterfactual_temp), size = 2)+ 
    facet_grid(.~counterfactual_temp) +
    scale_fill_manual(values = colorArray$tempContrast) + 
    scale_color_manual(values = colorArray$tempContrast) + 
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
                 paste0('g04_LagResponse_', sensitivity, '_', subSetVar, '_', 
                        subSet, '_', ERConstraint, "_", LRConstraint, "_", 
                        contrast, '.png')), 
      width = ww.fig*2, height = hh.fig*0.75, res =rr.fig*1)
  print(tag_facet(TP.a, 
                  tag_pool = paste(distinct(est.table,counterfactual_temp)$counterfactual_temp, ' deg C'), 
        x = -0.5, y = 0.75*max(est.table$uci_pc, na.rm=TRUE)))
  dev.off()
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Main Models
plot_LRCurve_perExposure('main', 'fullSet', 'fullSet', 'selected', 'selected', '05_95')
plot_LRCurve_perExposure('main', 'fullSet', 'fullSet', 'selected', 'selected', '05_25')
plot_LRCurve_perExposure('main', 'sex', 'f', 'selected', 'selected', '05_95')
plot_LRCurve_perExposure('main', 'sutter_county', 'sutter', 'selected', 'selected', '05_95')
plot_LRCurve_perExposure('main', 'sutter_county', 'not_sutter', 'selected', 'selected', '05_95')



