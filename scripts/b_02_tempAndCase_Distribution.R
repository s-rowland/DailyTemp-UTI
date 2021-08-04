# Examine Temperature Distribution Across Climate Regions
# Data Exploration
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 07/15/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Read Data 
# 2: Plot Temperature Distribution
# 3: Plot Case Distribution

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

# 0a Tell the analyst that the script is beginning 
StartTime_c_01 <- Sys.time()
print(paste('begin c_01 at', StartTime_c_01))

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####******************
#### 1: Read Data ####
####******************

# 1a Bring in the daily temperature dataset 
if(user == 'Analyst') {temper <- read_fst(here::here('data', 'intermediateData',
                              'daily_weather.fst')) }

if(user == 'Reviewer') {temper <- read_fst(here::here('data', 'intermediateData',
                                                     'fake_weather.fst')) }

# 1b Bring in climate region dataset 
clim <- read_csv(here::here('data', 'rawData', 
                            'noaa_clim_div.csv')) %>% 
  rename(fips = FIPS, ClimateRegion = NAME)

# 1c Combine 
dta <- inner_join(temper, clim, by = 'fips')

# 1d Read outcome data 
cases <- read_fst(here::here('data', 'preparedData',
                           paste0('cases_assignedTemp', outcomeName, '.fst')))
# 1e Combine 
cases <- inner_join(cases, clim, by = 'fips')


####**************************************
#### 2: Plot Temperature Distribution ####
####**************************************


ggplot(dta) + 
  geom_violin(aes(x=ClimateRegion, y =tmean, color = ClimateRegion)) + 
  labs(x='Climate Region')

dta.grp <- dta %>% 
  group_by(fips, ClimateRegion) %>%
  summarize(fipsMeanT =mean(tmean)) %>% 
  arrange(fipsMeanT)

dta.grp <- dta.grp %>% 
  mutate(fips_factor = factor(fips, dta.grp$fips))
dta <- dta %>%
  mutate(fips_factor = factor(fips, dta.grp$fips))

ggplot(dta) + 
  geom_violin(aes(x=fips_factor, y =tmean, color = ClimateRegion, fill=ClimateRegion)) + 
  labs(x='FIPS')

ggplot(dta.grp) + 
  geom_point(aes(x=fips_factor, y =fipsMeanT, color = ClimateRegion, fill=ClimateRegion)) + 
  labs(x='FIPS', y = 'FIPS Average Temp')


####*******************************
#### 3: Plot Case Distribution ####
####*******************************


ggplot(dta) + 
  geom_violin(aes(x=ClimateRegion, y =case_count, color = ClimateRegion)) + 
  labs(x='Climate Region')

dta.grp <- dta %>% 
  group_by(fips, ClimateRegion) %>%
  summarize(fipsMeanCases =mean(case_count)) %>% 
  arrange(fipsMeanCases)

dta.grp <- dta.grp %>% 
  mutate(fips_factor = factor(fips, dta.grp$fips))
dta <- dta %>%
  mutate(fips_factor = factor(fips, dta.grp$fips))

ggplot(dta) + 
  geom_violin(aes(x=fips_factor, y =case_count, color = ClimateRegion, fill=ClimateRegion)) + 
  labs(x='FIPS')

ggplot(dta.grp) + 
  geom_point(aes(x=fips_factor, y =fipsMeanCases, color = ClimateRegion, fill=ClimateRegion)) + 
  labs(x='FIPS', y = 'FIPS Average Number of Cases')
