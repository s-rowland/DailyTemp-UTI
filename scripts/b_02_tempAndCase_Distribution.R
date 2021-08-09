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

# 0a Create the folder structure, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_analysis.R'))
}

####******************
#### 1: Read Data ####
####******************

# 1a Read outcome data 
cases <- read_fst(here::here('data', 'prepared',
                           paste0('cases_assignedTemp_', outcomeName, '.fst')))

# 1b Generate set of observed temperatures
tempObs <- cases %>% 
  group_by(adate, fips) %>% 
  summarize(temp = mean(tLag00))

# 1c Bring in climate region dataset 
clim <- read_csv(here::here('data', 'from_adjacent_project', 
                            'noaa_clim_div.csv')) %>% 
  rename(fips = FIPS, climate_region = clim_div, county_name = NAME) %>% 
  mutate(climate_region = as.character(climate_region))

# 1d Combine 
cases <- inner_join(cases, clim, by = 'fips')
tempObs <- inner_join(tempObs, clim, by = 'fips')

####**************************************
#### 2: Plot Temperature Distribution ####
####**************************************

ggplot(tempObs) + 
  geom_violin(aes(x=climate_region, y =temp, color = climate_region)) + 
  labs(x='Climate Region')

tempObs.grp <- tempObs %>% 
  group_by(fips, climate_region) %>%
  summarize(fips_mean_temp = mean(temp)) %>% 
  arrange(fips_mean_temp)

tempObs.grp <- tempObs.grp %>% 
  mutate(fips_factor = factor(fips, tempObs.grp$fips))
tempObs <- tempObs %>%
  mutate(fips_factor = factor(fips, tempObs.grp$fips))

ggplot(tempObs) + 
  geom_violin(aes(x=fips_factor, y =temp, color = climate_region, fill=climate_region)) + 
  labs(x='FIPS')

ggplot(tempObs.grp) + 
  geom_point(aes(x=fips_factor, y =fips_mean_temp, color = climate_region, fill=climate_region)) + 
  labs(x='FIPS', y = 'FIPS Average Temp')


####*******************************
#### 3: Plot Case Distribution ####
####*******************************

ggplot(cases) + 
  geom_violin(aes(x=climate_region, y =case_count, color = climate_region)) + 
  labs(x='Climate Region')  

cases.grp <- cases %>% 
  group_by(fips, climate_region, county_name) %>%
  summarize(fips_mean_cases =mean(case_count)) %>% 
  arrange(fips_mean_cases)

cases.grp <- cases.grp %>% 
  mutate(fips_factor = factor(fips, cases.grp$fips))
cases <- cases %>%
  mutate(fips_factor = factor(fips, cases.grp$fips))

ggplot(cases) + 
  geom_violin(aes(x=fips_factor, y =case_count, color = climate_region, fill=climate_region)) + 
  labs(x='FIPS')

ggplot(cases.grp) + 
  geom_point(aes(x=fips_factor, y =fips_mean_cases, color = climate_region, fill=climate_region)) + 
  labs(x='FIPS', y = 'FIPS Average Number of Cases')


ggplot(cases.grp) + 
  geom_point(aes(x=fips_factor, y =fips_mean_cases, 
                 color = county_name, fill=county_name)) + 
  labs(x='FIPS', y = 'FIPS Average Number of Cases') + 
  scale_color_manual(values=c(as.vector(pals::stepped(24)), 
                       as.vector(pals::stepped2(12))))


####*******************************************
#### 4: Plot Case Distribution by Week/End ####
####*******************************************

# 4a Create Weekend variable 
cases <- cases %>% 
  mutate(ADMDateTime = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles')) %>%
  mutate(MM = month(ADMDateTime), 
       DoW = wday(ADMDateTime)) %>% 
  mutate(season = case_when(
    MM %in% c(12, 1, 2) ~'win', 
    MM %in% c(3, 4, 5) ~'spr', 
    MM %in% c(6, 7, 8) ~'sum', 
    MM %in% c(9, 10, 11) ~'fal'), 
    dow = case_when(
      DoW %in% c(1:5) ~ 'weekday', 
      DoW %in% c(6, 7) ~ 'weekend')) %>% 
  dplyr::select(-MM, DoW)

ggplot(cases) + 
  geom_violin(aes(x=dow, y =case_count, color = dow)) + 
  labs(x='Week/Weekend')

cases %>% 
  mutate(LA = if_else(fips == 6037, 'LA', 'notLA')) %>%
  group_by(dow, LA) %>% 
  summarize(MeanCases = mean(case_count), 
            SDCases = sd(case_count)) %>% 
  arrange(LA)

