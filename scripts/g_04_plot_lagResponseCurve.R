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
# In our datasetp, time is not smooth, but a step function - either you are in 
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
plot_laggedER <- function(Sensitivity, SubSetVar, SubSet, 
                          ERConstraint, LRConstraint, Contrast){
  # Sensitivity <- 'Main' ; SubSetVar <- 'FullSet'; SubSet <- 'FullSet'
  # Contrast <- '0595'
  
  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************
  
  # 1A.a Read model estimates
  est.table <- read_estimates(Sensitivity, SubSetVar, SubSet, 
                                      ERConstraint, LRConstraint, 'EstInd')
  
  # 1A.b Keep only relevant exposure contrast
  # 1A.b.i Determine the relevant Labels
  if(Contrast == '0595'){labelSet = c('per05', 'per95')}
  # 1A.b.ii Isolate the relevant exposure constrats
  est.table <- est.table %>% 
    filter(Label %in% labelSet)
  
  # 1A.c Wrangle estimates 
  est.table <- est.table %>% 
    dplyr::select(-Sensitivity, -SubSetVar, -SubSet, -IndCumul, -Label) %>%
    gather('LagName', 'Estimate', -CounterfactualTemp) %>% 
    mutate(VarName = str_sub(LagName, 1, 3), Lag = as.numeric(str_sub(LagName, 11))) %>%  
    dplyr::select(-LagName) %>%
    spread(VarName, Estimate) %>% 
    mutate(fit.pc = convert_to_percent(fit), 
           lci.pc = convert_to_percent(lci), uci.pc = convert_to_percent(uci)) 
  
  # 1A.d Convert CounterfactualTemp to character so that you can use discrete color scale
  est.table <- est.table %>% 
    mutate(CounterfactualTemp = as.character(round(CounterfactualTemp, 2)))
  
  ####*******************
  #### 1B: Make Plot ####
  ####******************* 
  
  # 1B.a Create plot 
  TP.a <- est.table %>%
    ggplot(aes(Lag)) + 
    geom_hline(yintercept=0, color ='grey' ) + 
    geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc, fill = CounterfactualTemp), 
                alpha  = 0.35, col=NA)+
    geom_line(aes(y = fit.pc, col = CounterfactualTemp), size = 2)+ 
    facet_grid(.~CounterfactualTemp) +
    scale_fill_manual(values = ColorArray$TempContrast) + 
    scale_color_manual(values = ColorArray$TempContrast) + 
    labs(y = paste0('Change in UTI Rate (%)'), 
         x = paste0('Days Since Exposure')) + 
    #coord_cartesian(ylim = c(Ymin, Ymax), expand = TRUE,
     #               default = FALSE, clip = 'off') + 
    #scale_y_continuous(breaks = seq(Ymin, Ymax, by = YStep)) +
   # scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35)) +
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
                 paste0('fullsetLagResponse_', Contrast, '.png')), 
      width = WW.fig*2, height = HH.fig*0.75, res =RR.fig*1)
  print(TP.a)
  dev.off()
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Main Models
plot_laggedER('Main', 'FullSet', 'FullSet', 'Selected', 'Selected', '0595')
