# Identify UTI Cases
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Read KP & Sutter Data
# 2: Aggregate UTI Cases
# 3: Apply Exclusion Criteria
# 4: Save Data

####**************
#### N: Notes #### 
####**************

# Na Description
# I am only including code for the parts of the data prep that I've done; 
# other data preparation steps could be included in this script, or placed in 
# their own script(s).
# I figured that there would be at least one script before matching controls; 
# so I created this empty script. 
# I made assumptions about what the other data preparation sections would be, 
# just so that I could have some placeholder section names. 

# this script could be combined with a_03 
# I found it easier to separate them so that I could run a_02 and make fake data
# for reproducibility

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
StartTime_a_01 <- Sys.time()
print(paste('begin a_01 at', StartTime_a_01))

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####******************************
#### 1: Read KP & Sutter Data ####
####******************************

# 1a Load the cases from both sources
load("M:/SH-ARES/Output for Joan/sutter_cases_final.rds")
load("M:/SH-ARES/Output for Joan/kpsc_cases_final.rds")

# 1b Read the dataset of counties in Sutter
sutter_counties <- read_csv(here::here('data', 'preparedData', 
                                            paste0('sutter_counties', '.csv')))

# 1c Fix FIPS codes
sut_reg_case2 <- sut_reg_case2 %>% 
  mutate(fips = as.numeric(as.character(county)))

kpsc_reg_case2 <- kpsc_reg_case2 %>%
  mutate(fips = county+6000)

# 1d Restrict the sut_cases to the Sutter catchement 
# and KPSC to not include Sutter catchment
#212940 - 212643 = lost 297 UTI cases by restricting to Sutter catchment
sut_reg_case3 <- sut_reg_case2 %>%
  dplyr::filter(fips %in% sutter_counties$COUNTYFP)

#673235 - 673078 = lost 157 by restricting to KP catchment
kpsc_reg_case3 <- kpsc_reg_case2 %>%
  dplyr::filter(!(fips %in% sutter_counties$COUNTYFP))

# 1e Select county and date of UTI and combine KP and Sutter UTI
sut_cross <- sut_reg_case3 %>% 
  ungroup() %>% 
  dplyr::select(adate, fips, female) %>% 
  mutate(sutter_county = "sutter county")
kpsc_cross <- kpsc_reg_case3 %>% 
  ungroup() %>%
  dplyr::select(adate, fips, female) %>%
  mutate(sutter_county = "not sutter")

####****************************
#### 2: Aggregate UTI Cases ####
####****************************

# 2a Combine Sutter and KP
county_combined <- rbind(sut_cross, kpsc_cross)

# 2b Aggregate to day-county level with count
cross_county_combined <- county_combined %>% 
  mutate(UTI = 1) %>%
  dplyr::group_by(fips, adate, sutter_county) %>% 
  dplyr::summarize(case_count=sum(UTI), 
                   case_count_sex_f = sum(female))

# 2c Check that aggregation was successful
check <- cross_county_combined %>% 
  group_by(fips, adate) %>% 
  summarise(n_count=n(), uti_count=sum(case_count), uti_count_female=sum(case_count_sex_f))
table(check$n_count)
head(check)
sum(check$uti_count)
sum(check$uti_count_female)

####*********************************
#### 3: Apply Exclusion Criteria ####
####*********************************

# no exclusion criteria
####******************
#### 4: Save Data ####
####******************

# 4a Save data 
# I use the fst format because it saves memory and it faster to read/write
cross_county_combined  %>% 
  write_fst(here::here('data', 'intermediateData', 
                       'cases_UTI.fst'))
