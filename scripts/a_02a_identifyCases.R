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
# 1: Read KP Data 
# 2: Identity UTI Cases
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

####*********************
#### 1: Read KP Data ####
####*********************

####***************************
#### 2: Identity UTI Cases ####
####***************************

####*********************************
#### 3: Apply Exclusion Criteria ####
####*********************************

####******************
#### 4: Save Data ####
####******************

# 4a Save data 
# I use the fst format because it saves memory and it faster to read/write
cases %>% 
  write_fst(here::here('data', 'intermediateData', 
                       'cases_UTI.fst'))

# 4b Tell the analyst that the script is done
cat('completed a_01 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_a_01), 1), ' min')
rm(StartTime_a_01)
