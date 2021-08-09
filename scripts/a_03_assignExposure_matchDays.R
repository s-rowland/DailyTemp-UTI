# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Wrangle Case Data 
# 2: Assign Lagged Exposure
# 3: Match Days
# 4: Add Variables
# 5: Save Data

####**************
#### N: Notes #### 
####**************

# Na Description
# I am only including code for the parts of the data prep that I've done; 
# other data preparation steps could be included in this script, or placed in 
# their own script(s).

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the script is beginning 
tic('Exposure assigned and days matched')

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

####**************************
#### 1: Wrangle Case Data ####
####**************************

# 1a Bring in either the real UTI data or the fake data
# the outPath parameter is set in subsection 0d of 
# 0_01_setUp_for_Analysis.R
cases <- read_fst(here::here('data', 'intermediate', 
                             paste0('cases_', outcomeName, '.fst'))) %>%  mutate(ZeroUTI = 'NonZero UTI') 


# 1b Add cells for date-county combinations 
# with zero UTI cases 
# these days will not show up in the analysis but are necessary for the analysis. 
# 1b.i Identify unique FIPS
uniqueFIPS <- cases %>% 
  dplyr::select(fips) %>% 
  distinct()
# 1b.ii Identify unique dates
uniqueDates <- cases %>% 
  dplyr::select(adate) %>% 
  distinct()
# 1b.iii Identify all FIP-date combinations
allCombos <- expand_grid(uniqueFIPS, uniqueDates) 
# 1b.iv Add the cases to the list of combinations
cases <- allCombos %>% 
  left_join(cases, by = c("fips", "adate"))
# 1b.v For any combination without a UTI count, we say we have 0 UTI
cases <- cases %>% 
  mutate(case_count = if_else(!is.na(ZeroUTI), as.numeric(case_count), 0),
         case_count_sex_f = if_else(!is.na(ZeroUTI), as.numeric(case_count_sex_f), 0)) %>% 
  dplyr::select(-ZeroUTI)
# 1b.vi Confirm that they got a zero. 
check <- cases %>% dplyr::filter(is.na(sutter_county))
check <- cases %>% dplyr::filter(is.na(case_count))
check <- cases %>% dplyr::filter(case_count==0)

# 1c Assign sutter_county status to the zero-UTI days 
sutter_list <- cases %>% 
  dplyr::select(fips, sutter_county) %>%
  distinct() %>% filter(!is.na(sutter_county))
cases <- cases %>% 
  dplyr::select(-sutter_county) %>% 
  inner_join(sutter_list, by = 'fips')

# 1d Create ADMDateTime variable 
# this variable is the ADMDT variable, but in datetime format 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
cases <- cases %>% 
  mutate(ADMDateTime = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) %>% 
  mutate(YYYY = year(ADMDateTime), 
         MM = month(ADMDateTime), 
         DoW = lubridate::wday(ADMDateTime, label=TRUE, abbr = TRUE))

# 1e Clean up 
rm(allCombos, uniqueDates, uniqueFIPS, check, sutter_list)
####*******************************
#### 2: Assign Lagged Exposure ####
####*******************************

# 2a Read exposure data 
if(outcomeName == 'UTI'){
  temper <- read_fst(here::here('data', 'intermediate', 'daily_weather.fst')) 
}
if(outcomeName == 'fake'){
  temper <- read_fst(here::here('data', 'intermediate', 'fake_weather.fst')) 
}

# 2b Process exposure data variables 
temper <- temper %>% 
  mutate(lagDate = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) 
temper$fips <- as.numeric(temper$fips)

# 2c Rename the cases dataframe 
dtaAssignedTemp <- cases

# 2d Assign exposure via a loop 
# if this step takes a long time we can find a way to parallelize it
# where each iteration is a new lag 
maxLag <- 22
for(l in 0:(maxLag-1)){
  dtaAssignedTemp <- assign_laggedExp(dtaAssignedTemp, temper, l)
}

# 2e Confirm that some counties do not have UTIs on every day 
dta_6041 <- dtaAssignedTemp %>% dplyr::filter(fips==6041)
head(dta_6041)

# NOTE: we should change the averaging period for RH if we use more lags
# 2f Create 7-day average rh
dtaAssignedTemp <- dtaAssignedTemp %>% 
  mutate((1/7) * (rLag00+rLag01+rLag02+rLag03+rLag04+rLag05+rLag06))

# 2g Clean up 
rm(cases, dta_6041)

####*******************
#### 3: Match Days ####
####*******************

# note that if you want to try multiple ways to match the days, 
# you can put this section of the code in the c_00a_analyze_dlnmTemp script 
# and let the sensitivity parameter (or make a new parameter) control 
# how matching is done. 
# We can use a much more simple approach for the matching if we are doing a 
# conditional Poisson model, since for that model we just need to match days 
# according to our criteria. 

# 3a Create matchID variable 
dtaAssignedTemp <- dtaAssignedTemp %>% 
  mutate(matchID = paste0(fips, '_', YYYY, MM, DoW)) %>% 
  dplyr::select(-YYYY, -MM, -DoW)

####**********************
#### 4: Add Variables ####
####**********************

# 4a Bring in climate region dataset 
clim <- read_csv(here::here('data', 'rawData', 
                            'noaa_clim_div.csv')) %>% 
  rename(fips = FIPS, climateRegion = NAME)

# 4b Join to data 
dta <- dta %>% 
  inner_join(clim, by = 'fips')

# Any further groupings we decide to do

####******************
#### 5: Save Data ####
####******************

# 5a Save data 
# I use the fst format because it saves memory and it faster to read/write
dtassignedTemp %>% 
  dplyr::select(adate, fips, matchID, contains('case_count'),
                contains('tLag'), contains("rLag"), meanRH) %>%
  write_fst(here::here('data', 'preparedData', 
                       paste0('cases_assignedTemp', outcomeName, '.fst')))

# 5b Clean up 
rm(l, maxLag, dtaAssignedTemp, temper)

# 5c Tell the analyst that the script is done
toc()
