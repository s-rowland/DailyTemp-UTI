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
# 1: Read Case Data 
# 2: Identify Matching Control Days
# 3: Assign Lagged Exposure
# 4: Save Data

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
StartTime_a_03 <- Sys.time()
print(paste('begin a_03 at', StartTime_a_03))

# 0b Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####***********************
#### 1: Read Case Data ####
####***********************

# 1a Bring in either the real UTI data or the fake data
# the outPath parameter is set in subsection 0d of 
# 0_01_setUp_for_Analysis.R
cases <- read_fst(here::here('data', 'intermediateData', 
                             paste0('cases_', outcomeName, '.fst')))

####****************************
#### 2: Create Matched Sets ####
####****************************

# note that if you want to try multiple ways to match the days, 
# you can put this section of the code in the c_00a_analyze_dlnmTemp script 
# and let the sensitivity parameter (or make a new parameter) control 
# how matching is done. 
# We can use a much more simple approach for the matching if we are doing a 
# conditional Poisson model, since for that model we just need to match days 
# according to our criteria. 

# 2a Create ADMDateTime variable 
# this variable is the ADMDT variable, but in datetime format 
# I prefer to keep a column of the date as a string, 
# because sometime excel will autocorrect dates to whatever excel thinks is the 
# right interpretation. 
cases <- cases %>% 
  mutate(ADMDateTime = parse_date_time(ADMDT, 'ymd', tz = 'America/Los_Angeles')) %>% 
  mutate(YYYY = year(ADMDateTime), 
         MM = month(ADMDateTime), 
         DoW = wday(ADMDateTime, label=TRUE, abbr = TRUE)) 

# 2b Create setID variable 
cases <- cases %>% 
  mutate(setID = paste0(fips, '_', YYYY, MM, DoW)) %>% 
  dplyr::select(-YYYY, -MM, -DoW) 
# you can keep these columns in to double check that the matching worked

####*******************************
#### 3: Assign Lagged Exposure ####
####*******************************

# 3a Rename the cases dataframe 
dta_assignedTemp <- cases

# 3b Read exposure data 
temper <- read_fst(here::here('data', 'intermediateData',
                                 'temperature_fips_fake.fst')) 

# 3c Process exposure data 
temper <- temper %>% 
  mutate(activeLag = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) 

# 3d Assign exposure via a loop 
# if this step takes a long time we can find a way to parallelize it
# where each iteration is a new lag 
maxLag <- 7
for(l in 0:(maxLag-1)){
  dta_assignedTemp <- assign_laggedExp(dta_assignedTemp, temper, l)
}

####******************
#### 4: Save Data ####
####******************

# 4a Save data 
# I use the fst format because it saves memory and it faster to read/write
dta_assignedTemp %>% 
  dplyr::select(ADMDT, fips, setID, case_count, contains('tLag')) %>%
  write_fst(here::here('data', 'preparedData', 
                       paste0('cases_assignedTemp', outcomeName, '.fst')))

# 4b Tell the analyst that the script is done
cat('completed a_03 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_a_03), 1), ' min\n')
rm(StartTime_a_03)
