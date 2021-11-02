# Generate Fake UTI Cases for Code Review
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

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

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

####**************************
#### 1: Create Fake Cases ####
####**************************

# 1a Set number of days of fake data 
nDays <- 366 + 365

# 1b Create a vector of fips Codes
activeFIPs <- read_fst(here::here('data', 'intermediate',
                                 'fake_weather.fst')) %>% 
  dplyr::select(fips) %>% 
  distinct() 

# 1c Read weather data
temper <- read_fst(here::here('data', 'intermediate',
                    'fake_weather.fst')) 

# 1d Begin dataset with Index variable
cases <- expand_grid(
  day_index = c(1:nDays),
  fips = activeFIPs$fips) 

# 1e Create Admit date variable 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
cases <- cases %>%  
  mutate(adate0 = parse_date_time('1/1/1999', 'mdy', tz = 'America/Los_Angeles') + 
           day_index*60*60*24) %>% 
  mutate(adate = str_sub(str_remove_all(adate0, '-'), 0, 8)) %>%
  dplyr::select(-contains('Index')) %>% 
  distinct()

# 1f Add temperature values
# here we assign some temperature values to create a fake effect
# we also put adate in the same format as in the real UTI data 
cases <- cases %>% 
  inner_join(temper, by = c('fips', 'adate')) %>% 
  mutate(tmeanLag1 = lag(tmean, 1), 
         tmeanLag2 = lag(tmean, 2),
         tmeanLag3 = lag(tmean, 3),
         tmeanLag4 = lag(tmean, 4),
         tmeanLag4 = lag(tmean, 4),
         tmeanLag4 = lag(tmean, 4),
         tmeanLag4 = lag(tmean, 4),
         tmeanLag4 = lag(tmean, 4)) %>% 
  mutate(adate = paste0(str_pad(month(adate0),2, 'left', '0'), 
                        str_pad(day(adate0),2, 'left', '0'), year(adate0))) 
  
# 1g Create case counts variable
# including a fake effect 
set.seed(1234)
cases$case_count <- floor(rnorm(nrow(cases), 20 + 0.015*cases$tmean + 
                                  0.005*cases$tmeanLag1+ 0.002*cases$tmeanLag2 + 
                                  0.001*cases$tmeanLag3+ 0.0005*cases$tmeanLag4, 2)) %>% 
  mutate(case_count = if_else(case_count < 0, 20, case_count)) %>%
  dplyr::select(-tmean, -avgrelhum) %>% 
  filter(complete.cases(cases))

# 1h Add some dates with zero UTI
cases <- cases %>% 
  mutate(case_count = if_else(row_number() == 111, 0, case_count))

# 1i Remove the zero cases 
cases <- cases %>% 
  filter(case_count != 0)

# 1j Add sutter county variable 
cases <- cases %>% 
  mutate(sutter_county = if_else(fips == 6041, "sutter", "kpsc"))

# 1k Add counts of female UTI and other modifiers 
cases <- cases %>% 
  mutate(case_count_sex_f = abs(case_count - 2), 
         case_count_low_ice = abs(case_count_sex_f / 2), 
         case_count_medicaid = abs(case_count_sex_f / 2))

# 1l Save data 
cases %>% 
  dplyr::select(fips, adate, sutter_county, case_count, case_count_sex_f, 
                case_count_low_ice,
                case_count_medicaid) %>%
  write_csv(here::here('data', 'intermediate', 
                                  'combined_cases_fake.csv'))

# 1m Cleanup 
rm(activeFIPs, nDays, cases)
