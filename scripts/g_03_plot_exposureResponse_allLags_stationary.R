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

# 1a Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 26, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 19, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 19, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16, angle = 0))

####*********************************
#### 2: Create Plotting Function ####
####*********************************

# 2a Name function 
plot_hourly_associations <- function(Sensitivity, SubSetVar, 
                                     ERConstraint, LRConstraint, SubSet){
  # Sensitivity <- 'Main' ; SubSetVar <- 'FullSet'; SubSet <- 'FullSet'
  
  # 2b Determine maximum lag 
  if(Sensitivity == 'Main'){maxLag <- 7}
  
  ####********************************
  #### 2A: Read Model Predictions ####
  ####********************************
  
  # 2A.a Read Table 
  est.table <- read_estimates(Sensitivity, SubSetVar, SubSet, 
                                      ERConstraint, LRConstraint, 'EstInd')
  
  # 2A.b Set up colors 
  LC.l <- topo.colors(maxLag)
  
  # 2A.c Determine percentages
  Temp.per05 <- quantile(temper$tmean, 0.05, type = 1)
  Temp.per95 <- quantile(temper$tmean, 0.95, type = 1)
  
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
                    paste0('_03a_allLagsStationary_', Sensitivity, '_', 
                    SubSetVar, '_', SubSet, '.pdf')))
     
  # 2B.c Create the lag-specific exposure-response plot for each lag
     # 2B.c.i Initialize loop
     for(i in 0:(maxLag-1)){
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
         geom_ribbon(aes(ymin= lci.pc,  ymax = uci.pc), fill = LC.l[i +1],  alpha  = 0.3)+
         geom_line(aes(y = fit.pc),  color = LC.l[i + 1])+ 
         labs(x = ' ', y = paste0('Change in \n UTI Rate (%)')) + 
         ggtitle(paste0( 'Exposure-Response \nfor ', i, ' Days Following Exposure'))+
        # coord_cartesian(ylim=c(-10, 10), expand = TRUE,
                         #default = FALSE, clip = 'off') + 
         #scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
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
plot_hourly_associations('Main', 'FullSet', 'Selected', 'Selected', 'FullSet')
