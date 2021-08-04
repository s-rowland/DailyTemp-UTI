# Set features for plots and create related objects
# Present Results
# Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# D: Descripton
# 0: Preparation
# 1: Set Common Plotting Features
# 2: Read Tables Required for Plotting

####********************
#### D: Description ####
####********************

# By setting common plotting features in a separate script 
# We can ensure consistency across plots 
# It also helps make some choices more explicit 
# eg, this is the script where I set the colors for subgroups

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts',
                    '0_01_setUp_for_Analysis.R'))
}

# 0b Add marker 
ran_g_01 <- 'ran_g_01'

####*************************************
#### 1: Set Common Plotting Features ####
####*************************************

# 1a Set common plot dimensions 
# not all figures are this size, but these are common sizes 
HH.fig <- 1000 
WW.fig <- 1000 
RR.fig <- 150

HH.efig <- 600 
WW.efig <- 600 
RR.efig <- 200

# 1b Create plotting theme 
# this winds up not being used because some of the figures contain multiple 
# plots, and thus their size needs to be uniquely chosen. 
#tema <-   theme_classic() +
  #theme(plot.title = element_text(size = 36, vjust = 1, hjust =0.5)) +
  #theme(axis.title.x = element_text(size = 16, vjust =0, lineheight=0.25)) + 
  #theme(axis.title.y = element_text(size = 16, angle = 90, vjust= 0.5)) + 
  #theme(axis.text.x = element_text(size = 12)) + 
 # theme(axis.text.y = element_text(size = 12, angle = 0))

# 1c Set individual colors
col.cold <- 'deepskyblue2'
col.hot <- 'tomato2'
col.modA <- 'burlywood4'
col.modB <- 'darkorchid1'

# 1d Set order of colors
# this is used in the scale_manual step of the plot 
# by setting the colors here, we can ensure consistent coloring scheme across 
# the manuscript
# if we do any subsetting, we can include those colors in this array
ColorArray <- list(
  Lag = terrain.colors(7),
  TempContrast = c(col.cold, col.hot),
  ModContrast = c(col.modA, col.modB)
)

# 1e Set order of sensitivity analyses
NameArray <- list(
  SensitivityCode = c('Main', 'RHnoAdj', 'RHdlnm', 'altConstraints',  '24LagHr', '48LagHr'),
  SensitivityManu = c('Main', 'No RH Adjustment','DLNM for RH', 'Alternative Constraints',
                      '24 Lag Hours', '48 Lag Hours')
)

# 1f Create plotting theme 
tema <-   theme_classic() +
  theme(plot.title = element_text(size = 26, vjust = 1, hjust =0.5)) +
  theme(axis.title.x = element_text(size = 19, vjust =0, lineheight=0.25)) + 
  theme(axis.title.y = element_text(size = 19, angle = 90, vjust= 0.5)) + 
  theme(axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16, angle = 0))

####******************************************
#### 2: Read Tables Required for Plotting ####
####******************************************

# 2a Read exposure data 
if(outcomeName == 'UTI'){
  temper <- read_fst(here::here('data', 'intermediateData', 'daily_weather.fst')) 
}
if(outcomeName == 'fake'){
  temper <- read_fst(here::here('data', 'intermediateData', 'fake_weather.fst')) 
}
# 2b Read selected models table 
SelectedModels <- read_csv(here::here(outPath, 'tables',
                                      'SelectedModels.csv'))
