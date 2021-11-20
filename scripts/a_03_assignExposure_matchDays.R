# Assign Temperature Exposure and Match Days
# Data Preparation
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

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

####********************
#### 0: Preparation #### 
####********************

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

# 0b Tell the analyst that the script is beginning 
tic('Exposure assigned and days matched')

####**************************
#### 1: Wrangle Case Data ####
####**************************

# 1a Bring in either the real UTI data or the toy data
# the outPath parameter is set in subsection 0d of 
# 0_01_setUp_for_Analysis.R
cases <- read_csv(here::here('data', 'intermediate', 
                             paste0('combined_cases_', outcomeName, '.csv')) , 
                  col_names = TRUE,
                  col_types = cols(
                    fips = col_character(), 
                    adate = col_character(), 
                    sutter_county = col_character(), 
                    case_count = col_double(), 
                    case_count_sex_f = col_double(), 
                    case_count_low_ice_f = col_double(), #vdo ts issue: the dta doesnt appear to have this var so I commented this out
                    case_count_medicaid = col_double())
                  ) %>% 
  mutate(ZeroUTI = 'NonZero UTI') %>% 
  rename(catchmentArea = sutter_county) 

# 1b Add high_ICE variable 
cases <- cases %>% 
  rename(case_count_ice_iceQ1 = case_count_low_ice_f) %>% 
  mutate(case_count_ice_iceQ234 = case_count - case_count_ice_iceQ1, 
         case_count_medicaid_medicaid = case_count_medicaid,
         case_count_medicaid_noMedicaid = case_count - case_count_medicaid)

# 1c Clean up the date variable
# only required for the real data - the two health-care systems manage their dates differently
if (outcomeName == 'UTI') {
  # 1c.i Clean up KP's date variable
  cases <- cases %>% 
    mutate(adate = parse_date_time(adate, 'ymd)'))
} else if (outcomeName == 'toy') {
  cases <- cases %>% 
    mutate(adate = parse_date_time(adate, 'mdy)'))
}

# 1d Add cells for date-county combinations 
# with zero UTI cases 
# these days will not show up in the analysis but are necessary for the analysis. 
# 1d.i Identify unique FIPS
uniqueFIPS <- cases %>% 
  dplyr::select(fips) %>% 
  distinct()
# 1d.ii Identify unique dates
uniqueDates <- cases %>% 
  dplyr::select(adate) %>% 
  distinct()
# confirm that it worked
#uniqueDates <- data.frame(adate = seq(ymd('2015-01-01'),ymd('2015-01-21'),by='days')) %>% 
 # bind_rows(uniqueDates)%>% 
  #distinct()
# 1d.iii Identify all FIP-date combinations
allCombos <- expand_grid(uniqueFIPS, uniqueDates) 
# 1d.iv Add the cases to the list of combinations
cases <- allCombos %>% 
  left_join(cases, by = c("fips", "adate"))
# 1d.v For any combination without a UTI count, we say we have 0 UTI
cases <- cases %>% 
  mutate(case_count = if_else(!is.na(ZeroUTI), as.numeric(case_count), 0),
         case_count_sex_f = if_else(!is.na(ZeroUTI), as.numeric(case_count_sex_f), 0),
         case_count_ice_iceQ1 = if_else(!is.na(ZeroUTI), as.numeric(case_count_ice_iceQ1), 0),
         case_count_ice_iceQ234 = if_else(!is.na(ZeroUTI), as.numeric(case_count_ice_iceQ234), 0),
         case_count_medicaid_medicaid = if_else(!is.na(ZeroUTI), as.numeric(case_count_medicaid_medicaid), 0)) %>% 
  dplyr::select(-ZeroUTI)
# 1d.vi Confirm that they got a zero. 
# should be non-zero
cases %>% dplyr::filter(is.na(catchmentArea)) %>% nrow()
cases %>% dplyr::filter(case_count==0) %>% nrow()
# should be zero 
cases %>% dplyr::filter(is.na(case_count)) %>% nrow()

# 1e Assign sutter_county status to the zero-UTI days 
sutter_list <- cases %>% 
  dplyr::select(fips, catchmentArea) %>%
  distinct() %>% 
  filter(!is.na(catchmentArea))
cases <- cases %>% 
  dplyr::select(-catchmentArea) %>% 
  inner_join(sutter_list, by = 'fips')
# confirm success
cases %>% filter(is.na(catchmentArea)) %>% nrow()
# 1f Create ADMDateTime variable 
# this variable is the ADMDT variable, but in datetime format 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
cases <- cases %>% 
  mutate(ADMDateTime = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) %>% 
  mutate(YYYY = year(ADMDateTime), 
         MM = month(ADMDateTime), 
         DoW = lubridate::wday(ADMDateTime, label=TRUE, abbr = TRUE))

# 1g Clean up 
rm(allCombos, uniqueDates, uniqueFIPS, sutter_list)

####*******************************
#### 2: Assign Lagged Exposure ####
####*******************************

# 2a Read exposure data 
if(outcomeName == 'UTI'){
  temper <- read_fst(here::here('data', 'intermediate', 'daily_weather.fst')) 
}
if(outcomeName == 'toy'){
  temper <- read_fst(here::here('data', 'intermediate', 'toy_weather.fst')) 
}

# 2b Process exposure data variables 
temper <- temper %>% 
  mutate(lagDate = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) 
temper$fips <- as.character(temper$fips)

# 2c Rename the cases dataframe 
dtaAssignedTemp <- cases

# 2d Assign exposure via a loop 
# if this step takes a long time we can find a way to parallelize it
# where each iteration is a new lag 
maxLag <- 21
for(l in 0:(maxLag)){
  dtaAssignedTemp <- assignLaggedExp(dtaAssignedTemp, temper, l)
}

# 2e Confirm that some counties do not have UTIs on every day
# note: in the real data, fips 6041 has days with zero UTI cases
dta_6041 <- dtaAssignedTemp %>% dplyr::filter(fips==6041)
head(dta_6041)

# 2f Create 14-day average rh
# str: modified to be 14 days average
dtaAssignedTemp <- dtaAssignedTemp %>% 
  mutate(meanRH = (1/14) * (rLag00+rLag01+rLag02+rLag03+rLag04+rLag05+rLag06+
                              rLag07+rLag08+rLag09+rLag10+rLag11+rLag12+rLag13), 
         meanRH21 = (1/21) * (rLag00+rLag01+rLag02+rLag03+rLag04+rLag05+rLag06+
                              rLag07+rLag08+rLag09+rLag10+rLag11+rLag12+rLag13+
                                rLag14+rLag15+rLag16+rLag17+rLag18+rLag19+rLag20))

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

# Note: climate regions were used in some exploratory analyses but not included 
# in the manuscript

# 4a Bring in climate region dataset 
clim <- read_csv(here::here('data', 'raw', 
                            'noaa_clim_div.csv')) %>% 
  rename(fips = FIPS, climateRegion = NAME) %>% 
  mutate(fips = as.character(fips))

# 4b Join to data 
dtaAssignedTemp <- dtaAssignedTemp %>% 
  inner_join(clim, by = 'fips')

####******************
#### 5: Save Data ####
####******************

# 5a Save data 
# I use the fst format because it saves memory and it faster to read/write
dtaAssignedTemp %>% 
  dplyr::select(adate, fips, matchID, contains('case_count'),
                contains('tLag'), contains("rLag"), meanRH, meanRH21, catchmentArea) %>%
  write_fst(here::here('data', 'prepared', 
                       paste0('cases_assignedTemp_', outcomeName, '.fst')))

# 5b Clean up 
rm(l, maxLag, dtaAssignedTemp, temper)

# 5c Tell the analyst that the script is done
toc()
