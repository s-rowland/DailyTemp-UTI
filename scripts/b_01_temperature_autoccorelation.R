# Assign Exposure
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Read Data 
# 2: Function to Calculate County-Specific Autocorrelation
# 3: Calculate Autocorrelation 
# 4: Plot Autocorrelation

####**************
#### N: Notes #### 
####**************

# Na Description
# I am only including code for the parts of the data prep that I've done; 
# other data preparation steps could be included in this script, or placed in 
# their own script(s).
# I made assumptions about what the other data preparation sctions would be, 
# just so that I could have some placeholder section names

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the daily temperature dataset 
if(user == 'Analyst') {temper <- read_fst(here::here('data', 'intermediate',
                              'daily_weather.fst')) }

if(user == 'Reviewer') {temper <- read_fst(here::here('data', 'intermediate',
                                                     'fake_weather.fst')) }

####**************************************************************
#### 2: Function to Calculate County-Specific Autocorrelation ####
####**************************************************************

calcAutoCorr <- function(dataset){

  dta <- dataset %>% 
    arrange(adate) %>% 
    mutate(tlag00 = lag(tmean, 0),
           tlag01 = lag(tmean, 1),
           tlag02 = lag(tmean, 2),
           tlag03 = lag(tmean, 3),
           tlag04 = lag(tmean, 4),
           tlag05 = lag(tmean, 5),
           tlag06 = lag(tmean, 6),
           tlag07 = lag(tmean, 7),
           tlag08 = lag(tmean, 8),
           tlag09 = lag(tmean, 9), 
           tlag10 = lag(tmean, 10), 
           tlag11 = lag(tmean, 11), 
           tlag12 = lag(tmean, 12), 
           tlag13 = lag(tmean, 13), 
           tlag14 = lag(tmean, 14), 
           tlag15 = lag(tmean, 15)) 

  dta <- dta %>% filter(complete.cases(dta))
  
  dta.means <- dta %>% group_by(fips) %>% 
    summarize(corr00 = cor(tlag00, tlag00), 
           corr01 = cor(tlag00, tlag01), 
           corr02 = cor(tlag00, tlag02), 
           corr03 = cor(tlag00, tlag03), 
           corr04 = cor(tlag00, tlag04), 
           corr05 = cor(tlag00, tlag05), 
           corr06 = cor(tlag00, tlag06), 
           corr07 = cor(tlag00, tlag07), 
           corr08 = cor(tlag00, tlag08), 
           corr09 = cor(tlag00, tlag09), 
           corr10 = cor(tlag00, tlag10), 
           corr11 = cor(tlag00, tlag11), 
           corr12 = cor(tlag00, tlag12), 
           corr13 = cor(tlag00, tlag13), 
           corr14 = cor(tlag00, tlag14), 
           corr15 = cor(tlag00, tlag15)) %>% 
    ungroup()
  
  return(dta.means)

}

####**********************************
#### 3: Calculate Autocorrelation ####
####**********************************

# 3a Split the data 
temper.fips <- split(temper, temper$fips)

# 3b Calculate the autocorrelation
corrs <- map(temper.fips, calcAutoCorr) %>% 
  bind_rows()

# 3c Determine mean temporal autocorr 
corrs.long <- corrs %>% 
  gather('Lag', 'Corr', -fips) %>% 
  group_by(Lag) %>% 
  summarize(mean_corr = mean(Corr)) %>% 
  ungroup() %>% 
  mutate(Lag = as.numeric(str_sub(Lag, 5)))

corrs.long %>% 
  write_csv(here::here(outPath, 'tables', 
                       paste0('temporal_autocorr', '.csv')))

corrs.final <- read_csv(here::here(outPath, 'tables', 
                                      paste0('temporal_autocorr', '.csv')))


corrs.final <- data.frame(x = 1:nrow(corrs.long))
corrs.final$Lag <- corrs.long$Lag
corrs.final$mean_corr <- corrs.long$mean_corr

####*****************************
#### 4: Plot Autocorrelation ####
####*****************************

png(here::here(outPath, 'plots', 
               paste0('temporal_autocorr', '.png')))

ggplot(corrs.long) + 
  geom_point(aes(x=Lag, y= mean_corr), color = 'orange', fill = 'orange') + 
  geom_line(aes(x=Lag, y= mean_corr)) + 
  scale_x_continuous(breaks=0:15)
dev.off()