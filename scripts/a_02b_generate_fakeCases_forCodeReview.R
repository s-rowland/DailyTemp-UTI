# Generate Fake UTI Cases for Code Review
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Fake Cases

####**************
#### N: Notes #### 
####**************

# Na Description
# The goal of this script is to create a fake dataset for reproducibility testing
# this fake dataset should have a null association between temp and UTI

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_a_01b <- Sys.time()
print(paste('begin a_01b at', StartTime_a_01b))

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts',
                    '0_01_setUp_for_Analysis.R'))
}

####**************************
#### 1: Create Fake Cases ####
####**************************

# 1a Set number of days of fake data 
NDays <- 366 + 365

# 1b Create a vector of fips Codes
# In the final code we should just pull fips Codes from real exposure dataset 
activefips <- read_fst(here::here('data', 'intermediateData',
                                 'temperature_fips_fake.fst')) %>% 
  dplyr::select(fips) %>% 
  distinct() 

# 1c Begin dataset with Index variable
cases <- expand_grid(
  dIndex = c(1:NDays),
  fips = activefips$fips)

# 1d Create case counts variable
set.seed(1234)
cases$case_count <- floor(rnorm(nrow(cases), 10 + 2* sin(2*pi*cases$dIndex / 366), 2))
     
# 1e Create Admit date variable 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
cases <- cases %>%  
  mutate(ADMDT = parse_date_time('1/1/1999', 'mdy', tz = 'America/Los_Angeles') + 
           dIndex*60*60*24) %>% 
  mutate(ADMDT = str_sub(str_remove_all(ADMDT, '-'), 0, 8)) %>%
  dplyr::select(-contains('Index')) %>% 
  distinct()

# 1f Save data 
cases %>% 
  write_fst(here::here('data', 'intermediateData', 
                                  'cases_fake.fst'))
