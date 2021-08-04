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
# 2: Fit Model
# 3: Save Results

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

# 1a Bring in the dataset of the matched days with exposure
dta <- read_fst(here::here('data', 'preparedData',
                           paste0('cases_assignedTemp', outcomeName, '.fst')))

# 1b Bring in the daily temperature dataset 
if(outcomeName == 'UTI'){
  temper <- read_fst(here::here('data', 'intermediateData', 'daily_weather.fst')) 
}
if(outcomeName == 'fake'){
  temper <- read_fst(here::here('data', 'intermediateData', 'fake_weather.fst')) 
}

####******************
#### 2: Fit Model ####
####******************

# 2a Create Model Name
ModelName <- paste('noConstraint', 'FullSet', 'FullSet', 
                   '3dfevenknots', 'free', sep = '_')

# 2b Create OneBases 
ob.temp00 <- onebasis(dta$tLag00, fun = 'ns', df = 3)
ob.temp01 <- onebasis(dta$tLag01, fun = 'ns', df = 3)
ob.temp02 <- onebasis(dta$tLag02, fun = 'ns', df = 3)
ob.temp03 <- onebasis(dta$tLag03, fun = 'ns', df = 3)
ob.temp04 <- onebasis(dta$tLag04, fun = 'ns', df = 3)
ob.temp05 <- onebasis(dta$tLag05, fun = 'ns', df = 3)
ob.temp06 <- onebasis(dta$tLag06, fun = 'ns', df = 3)

# 2c Convert setID to a factor 
# the gnm() function, which we use for the conditional poission 
# requires the matching variable to have factor format
dta <- dta %>% 
  mutate(matchID = as.factor(matchID)) %>% 
  arrange(matchID)

# 2d Fit Model 
mod <- gnm(case_count ~ ob.temp00 + ob.temp01 + ob.temp02 + ob.temp03 + 
             ob.temp04 + ob.temp05 + ob.temp06, 
           family = quasipoisson(link= 'log'), 
           data = dta, 
           eliminate = matchID)

####*********************
#### 3: Save Results ####
####*********************

# 3a Save the model 
mod %>% saveRDS(here::here(outPath, 'models',
                           paste0(ModelName, '.RDS')))

# 3b Create set of counterfactual exposures for comparison 
# with crosspred, we specify the reference level and the counterfactual 
# exposure levels 
# we use the Label variable to track these exposure levels later on
expContrasts <- data.frame(  
  CounterfactualTemp = c(seq(min(temper$tmean), max(temper$tmean), length.out = 100), 
                         quantile(temper$tmean, 0.01, type = 1), quantile(temper$tmean, 0.99, type = 1), 
                         quantile(temper$tmean, 0.05, type = 1), quantile(temper$tmean, 0.95, type = 1), 
                         quantile(temper$tmean, 0.10, type = 1), quantile(temper$tmean, 0.90, type = 1),  
                         quantile(temper$tmean, 0.15, type = 1), quantile(temper$tmean, 0.85, type = 1),
                         quantile(temper$tmean, 0.20, type = 1), quantile(temper$tmean, 0.80, type = 1), 
                         quantile(temper$tmean, 0.25, type = 1), quantile(temper$tmean, 0.75, type = 1), 
                         mean(temper$tmean) - sd(temper$tmean), 
                         mean(temper$tmean) + sd(temper$tmean), 
                         mean(temper$tmean) - 10,  mean(temper$tmean) + 10),
  Label = c(rep('ERValues', 100), 'per01','per99', 'per05', 'per95', 'per10', 'per90',
            'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'MeanMinusSD', 'MeanPlusSD', 
            'MeanMinus10', 'MeanPlus10')) %>% 
  mutate(CounterfactualTemp = round(CounterfactualTemp, 7)) %>% 
  arrange(CounterfactualTemp)

# 3c Combine effect estimates 
# dirty version for now

est <- crosspred(basis = ob.temp00,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag0 <- est$allRRfit
expContrasts$lci.rr.lag0 <- est$allRRlow
expContrasts$uci.rr.lag0 <- est$allRRhigh
est <- crosspred(basis = ob.temp01,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag1 <- est$allRRfit
expContrasts$lci.rr.lag1 <- est$allRRlow
expContrasts$uci.rr.lag1 <- est$allRRhigh
est <- crosspred(basis = ob.temp02,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag2 <- est$allRRfit
expContrasts$lci.rr.lag2 <- est$allRRlow
expContrasts$uci.rr.lag2 <- est$allRRhigh
est <- crosspred(basis = ob.temp03,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag3 <- est$allRRfit
expContrasts$lci.rr.lag3 <- est$allRRlow
expContrasts$uci.rr.lag3 <- est$allRRhigh
est <- crosspred(basis = ob.temp04,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag4 <- est$allRRfit
expContrasts$lci.rr.lag4 <- est$allRRlow
expContrasts$uci.rr.lag4 <- est$allRRhigh
est <- crosspred(basis = ob.temp05,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag5 <- est$allRRfit
expContrasts$lci.rr.lag5 <- est$allRRlow
expContrasts$uci.rr.lag5 <- est$allRRhigh
est <- crosspred(basis = ob.temp06,model = mod,cen = mean(temper$tmean),
                 at = expContrasts$CounterfactualTemp, cumul=TRUE, bylag=0.2)
expContrasts$fit.rr.lag6 <- est$allRRfit
expContrasts$lci.rr.lag6 <- est$allRRlow
expContrasts$uci.rr.lag6 <- est$allRRhigh

# 3d Save combined estimates
expContrasts %>% 
  write.csv(here::here(outPath, 'estimates', 
                       paste0('EstInd_', ModelName, '.csv')))

