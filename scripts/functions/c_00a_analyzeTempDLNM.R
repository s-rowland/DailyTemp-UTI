# Analyze Association between Temperature and UTI
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Begin Function
# 2: Create Variables
# 3: Ascertain Cases and Stratify by Patient Characteristics
# 4: Stratify Data by Temporal and Spatial Factors
# 5: Create Crossbasis
# 6: Fit Health Model 
# 7: Save Results
  # 7A: Save Model AIC
  # 7B: Save Model Results

####**************
#### N: Notes #### 
####**************

# Na Description
# This code is the function for fitting all of the models 
# for the analysis 
# except for the negative control exposure sensitivity analysis 
# By using the same code, we can ensure that the exact same dataprocessing 
# and output is applied to each model, 
# without updating many individual codes 
# The code outputs a) the model, b) estimate for the mean to 10th and 90th percentile 
# c) for penalized spline models only, an ER plot.

# Nb modelName 
# A critical object in this project is the modelName 
# This identifiers provides all the unique information about a model 
# When you look a file name, you know exactly what the model is about

####********************
#### 0: Preparation #### 
####********************

####***********************
#### 1: Begin Function ####
####***********************

# 1a Name function
analyzeTempDLNM <- function(sensitivity, subSetVar, subSet,
                             ERConstraint, LRConstraint, saveModel){
   
  # sensitivity <- 'main'; 
  # ERConstraint <- '3dfEvenKnots'; LRConstraint <- '3dfLogKnots';
  # subSetVar <- 'fullSet'; subSet <- 'fullSet';  saveModel <- 'saveAIC'
  
  #sensitivity <- 'explor'; 
  #ERConstraint <- '3dfEvenKnots'; LRConstraint <- '3dfLogKnots';
  #subSetVar <- 'catchementArea'; subSet <- 'kpsc';  saveModel <- 'saveAIC'
  
# set this instead to test subSetting by a patient characteristic
  # subSetVar <- 'medicaid'; subSet <- 'medicaid';  saveModel <- 'saveAIC'
  
  # set this instead to test subSetting by a time-varying characteristic
  # subSetVar <- 'season'; subSet <- 'sum';  saveModel <- 'saveAIC'
  
  # 1b Create modelName 
  modelName <- paste(sensitivity, subSetVar, subSet, 
                     ERConstraint, LRConstraint, sep = '_')
  
  ####**************************
  #### 2: Create Variables ####
  ####*************************
  
# Here you can create any variables you want - a term for long-term trends, etc
  # or seasons if you want to sub-divide the data by season
  
  # 2a Create secular time variable 
  dta <- dta %>% 
    mutate(ADMDateTime = adate)
    #mutate(ADMDateTime = parse_date_time(adate, 'ymd', tz = 'America/Los_Angeles'))
  
  # 2b Here I make date variables
  minDateTime = min(dta$ADMDateTime)
  dta <- dta %>% 
    mutate(day_index = as.numeric(ADMDateTime - minDateTime, 'days'))
  dta <- dta %>% 
    mutate(MM = month(ADMDateTime), 
           DoW = wday(ADMDateTime)) %>% 
    mutate(season = case_when(
      MM %in% c(12, 1, 2) ~'win', 
      MM %in% c(3, 4, 5) ~'spr', 
      MM %in% c(6, 7, 8) ~'sum', 
      MM %in% c(9, 10, 11) ~'fal'), 
      dow = case_when(
        DoW %in% c(1:5) ~ 'wk', 
        DoW %in% c(6, 7) ~ 'wknd')) %>% 
    dplyr::select(-MM, DoW)
  
  ####****************************************************************
  #### 3: Ascertain Cases and Stratify by Patient Characteristics ####
  ####****************************************************************
  
  # if you considered alternative UTI criteria, you can change which count column
  # you analyze. 
  # in the analysis script, we use the variable 'outcome_count' as the 
  # dependent variable 
  # so in this section we just set the outcome_count variable to whatever column 
  # we can to use 
  
  # note that we can also use this section to do subSetting by patient 
  # characteristics, for example, we could have a column of counts for UTI from 
  # subjects over 65, and a column of counts for subjects under 65. 
  # for such subSetting, I think it would be better to keep the sensitivity 
  # as 'main' and instead vary the subSet parameters 
  # so that the sensitivity term only distinguishes the main results 
  # from sensitivity results. 
  
  # we would also need to add any other temporal or spatial subSets to the 
  # if statement
  if(subSetVar %in% c('fullSet', 'season', 'catchmentArea', 'dow')){
    dta <- dta %>% mutate(outcome_count = case_count_sex_f)
    } else {
    countVar = paste0('case_count_', subSetVar, '_', subSet)
    dta$outcome_count <- dta[, countVar]
  }
  # special female-only sensitivity analyses so that we can examine the effect #vdo comment: is this description accurate? We are looking at female & male here; also wasn't the study pop limited to female only?
  # of restricting to females when doing effect modification
  if(sensitivity == 'FandM'){
    dta <- dta %>% 
      mutate(outcome_count = case_count)
  }
  
  ####******************************************************
  #### 4: Stratify Data by Temporal and Spatial Factors ####
  ####******************************************************

  # 4a Apply any stratification by time-varying or spatially-varying factors
  # in this section, we first rename the column for that subSetting variable 
  # to subSetVar 
  # we then apply a filter to only keep observations that match the particular 
  # subSet we are interested in. 
  
  # Right now it is set up to only subSet for the season variable 
  # if we create such a variable 
  # if we want to add other variable, we can just add them to the if statement
  if(subSetVar == 'season' | subSetVar == 'catchmentArea' | subSetVar == 'dow'){
    dta <- dta %>% 
      rename(subSetVar = !!subSetVar) %>%
      filter(subSetVar == subSet)
  }
  
  ####**************************
  #### 5: Create Crossbasis ####
  ####**************************
  
  # 5a Convert setID to a factor 
  # the gnm() function, which we use for the conditional poission 
  # requires the matching variable to have factor format
  dta <- dta %>% 
    mutate(matchID = as.factor(matchID)) %>% 
    arrange(matchID)
  
  # 5b Set the number of lags based on the sensitivity 
  if(!str_detect(sensitivity, 'DayLag')){numLag <- 14
  }else {numLag <- as.numeric(str_remove_all(sensitivity, '[A-z]'))}
  
  # 5c Set ER and LR constraints
  # we remove all of the letters from the  parameter, leaving just the number of df
  ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]'))
  LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))
  
  # 5d Isolate the exposure during the lags of interest
  exposure_profiles <- as.matrix(dplyr::select(dta, contains('tLag')))[,1:numLag]
  
  # 5e Create crossbasis
  # Here I include more options than I use in the main model 
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'EvenKnots')){
    cb.temp <- crossbasis(
      exposure_profiles, 
      lag=c(0,(numLag-1)), # we subtract 1 because lags start at 0
      argvar=list(fun='lin'),
      arglag=list(fun='ns', df = LRdf))}
  if(str_detect(ERConstraint, 'EvenKnots') & str_detect(LRConstraint, 'EvenKnots')){
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ns', df = LRdf))}
  if(str_detect(ERConstraint, 'EvenKnots') & str_detect(LRConstraint, 'LogKnots')){
    lagKnots <- logknots(numLag-1, fun = 'ns', df = LRdf)
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(knots=lagKnots))}
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'LogKnots')){
    lagKnots <- logknots(numLag-1, fun = 'ns', df = LRdf)
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='lin'),
      arglag=list(knots=lagKnots))}
  if(str_detect(ERConstraint, 'psp') & !str_detect(LRConstraint, 'psp')){
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='ps', df =9),
      arglag=list(fun='ns', df = LRdf))}
  if(!str_detect(ERConstraint, 'psp') & str_detect(LRConstraint, 'psp')){
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='ps'))}
  if(str_detect(sensitivity, 'lagBspline')){
    cb.temp <- crossbasis(
      exposure_profiles,
      lag=c(0,(numLag-1)),
      argvar=list(fun='ns', df = ERdf),
      arglag=list(fun='bs'))}
  
  ####*************************
  #### 6: Fit Health Model ####
  ####*************************
  
  # 6a Fit Main Model
  # note that we can change the distribution family to poisson if the dispersion 
  # factor is 1.
      mod <- gnm(outcome_count ~ cb.temp + meanRH, 
             family = quasipoisson(link= 'log'), 
             data = dta, 
             eliminate = matchID)# eliminate is the matching variable
 
  
  # 6b Fit alternative models
  # this is just an example to show what it would look like. #vdo comment: even though it's an example, you do end up using it in as a sensitivity analyses no? The comment is a bit misleading and implies the below is unimportant
  if(sensitivity == 'noRH'){
    mod <- gnm(outcome_count ~ cb.temp, 
               family = quasipoisson(link= 'log'), 
               data = dta, 
               eliminate = matchID) 
  }
  if(sensitivity == 'RHdlnm'){
    cb.rh <- crossbasis(
      as.matrix(dplyr::select(dta, contains('rLag')))[,1:numLag], 
      lag=c(0,(numLag-1)),
      argvar=list(fun='ns', df = 3),
      arglag=list(fun='ns', df = 3))
    mod <- gnm(outcome_count ~ cb.temp + cb.rh, 
               family = quasipoisson(link= 'log'), 
               data = dta, 
               eliminate = matchID) 
  }

  ####*********************
  #### 7: Save Results ####
  ####*********************
  
  ####************************
  #### 7A: Save Model AIC ####
  ####************************
  
  # 7A.a Begin option
  # when we do the grid search, we want to just save the model's AIC and not its results
  if(saveModel == 'saveAIC'){
    
    # 7A.b Readin the table of AIC's
    aic.table <- read_csv(here::here(outPath, 'tables', 
                                      'Model_AIC.csv'), col_types = 'cccccdcT')
    
    # 7A.c Calculate QAIC
    # 7A.c.i Make a version of the quasipoisson distribution that includes the AIC 
    # that you would get if the distribution were poission #vdo comment: what you're doing here is clear, but could you clarify the why? ie - why can't we directly calculate the qaic right off the bat? This comes from my personal lack of understanding/remembering AIC for poisson and QAIC for quasipoisson
    x.quasipoisson <- function(...) {
      res <- quasipoisson(...)
      # This line is telling R to compute AIC for quasipoission
      # as it would for a model with poisson distribution
      res$aic <- poisson(...)$aic
      res
    }
    # 7A.c.ii Update the model with the new distribution
    mod1 <- update(mod,family = 'x.quasipoisson')
    # 7A.c.iii Extract the overdispersion factor
    qaic.mod <- QAIC(mod1, chat = summary(mod1)$dispersion)
    # reminder: equation for dispersion factor
    #with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
    
    # 7A.d Add this aic to the set of AIC's
    aic.table[1+nrow(aic.table),] <- list(sensitivity, subSetVar, subSet,
                                          ERConstraint, LRConstraint, qaic.mod,
                                          NA, Sys.time())
    
    # 7A.e Remove any old AICs and then save
    # at the slice step you keep only the earliest AIC for each model-constraint combo
    aic.table %>% 
      group_by(sensitivity, subSetVar, subSet,ERConstraint, LRConstraint) %>% 
      arrange(desc(run_date)) %>% 
      slice(0:1) %>% 
      filter(!is.na(sensitivity)) %>%
      write_csv(here::here(outPath, 'tables',
                           'Model_AIC.csv'))
  }
  
  ####****************************
  #### 7B: Save Model Results ####
  ####****************************
  
  # We only save the model results for the model with the selected constraints 
  # this saves us some memory and makes the folders easier to manage. 
  # It also logically separates the constraint selection process and the 
  # final model estimation
  
  # 7B.a Begin option  
  if(saveModel == 'saveModel'){
    
    # 7B.b Save the model 
    mod %>% saveRDS(here::here(outPath, 'models',
                               paste0(modelName, '.RDS')))

    # 7B.c Create set of counterfactual exposures for comparison 
    # with crosspred, we specify the reference level and the counterfactual 
    # exposure levels 
    # we use the Label variable to track these exposure levels later on
    # create set of temperatures
    
    expContrasts <- data.frame(  
      counterfactual_temp = c(seq(min(tempObs$temp), max(tempObs$temp), length.out = 100), 
                              mean(tempObs$temp),
                             quantile(tempObs$temp, 0.01, type = 1), quantile(tempObs$temp, 0.99, type = 1), 
                             quantile(tempObs$temp, 0.05, type = 1), quantile(tempObs$temp, 0.95, type = 1), 
                             quantile(tempObs$temp, 0.10, type = 1), quantile(tempObs$temp, 0.90, type = 1),  
                             quantile(tempObs$temp, 0.15, type = 1), quantile(tempObs$temp, 0.85, type = 1),
                             quantile(tempObs$temp, 0.20, type = 1), quantile(tempObs$temp, 0.80, type = 1), 
                             quantile(tempObs$temp, 0.25, type = 1), quantile(tempObs$temp, 0.75, type = 1),
                             mean(tempObs$temp) - sd(tempObs$temp), 
                             mean(tempObs$temp) + sd(tempObs$temp), 
                             mean(tempObs$temp) - 10,  mean(tempObs$temp) + 10),
      label = c(rep('ERValues', 100), 'mean', 'per01','per99', 'per05', 'per95', 'per10', 'per90',
                'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'meanMinusSD', 'meanPlusSD', 
                'meanMinus10', 'meanPlus10')) %>% 
      mutate(counterfactual_temp = round(counterfactual_temp, 7))
    
    # 7B.d Generate estimates
    # the cen argument sets the reference exposure level for our effect estimates
    # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
    # right now the reference exposure level is the mean temperature 
    # but after reviewing the real results, we could choose a different value 
    # like the temperature at which there is minimal risk
    # or the temperature where the curve changes shape
    
    est.ref.mean <- crosspred(basis = cb.temp,
                      model = mod,
                      cen = mean(tempObs$temp), # quantile(tempObs$temp, 0.10, type = 1), # 
                      at = expContrasts$counterfactual_temp, 
                      cumul=TRUE, 
                      bylag=0.2)
  
    est.ref.per05 <- crosspred(basis = cb.temp,
                             model = mod,
                             cen = quantile(tempObs$temp, 0.05, type = 1), # quantile(tempObs$temp, 0.10, type = 1), # 
                             at = expContrasts$counterfactual_temp, 
                             cumul=TRUE, 
                             bylag=0.2)
    
    
    # 7B.e Extract coefficient fit and CI 
    fit.table0 <- as.data.frame(est.ref.mean$matRRfit) %>% 
      mutate(refT = 'mean') 
    
    fit.table <- fit.table0 %>% 
      bind_rows(as.data.frame(est.ref.per05$matRRfit) %>% mutate(refT = 'per05'))
    
    colnames(fit.table) <- paste0('fit.rr.', colnames(fit.table))
    fit.table <- fit.table %>%  
      rename(refT = fit.rr.refT) %>%
      mutate(counterfactual_temp = rep(as.numeric(row.names(fit.table0)), 2))

    lci.table <- as.data.frame(est.ref.mean$matRRlow) %>% 
      bind_rows(as.data.frame(est.ref.per05$matRRlow))
    colnames(lci.table) <- paste0('lci.rr.', colnames(lci.table))
    
    uci.table <- as.data.frame(est.ref.mean$matRRhigh) %>% 
      bind_rows(as.data.frame(est.ref.per05$matRRhigh) )
    colnames(uci.table) <- paste0('uci.rr.', colnames(uci.table))
    
    # 7B.f Combine fit and se for individual lags 
    # note that all RR are relative to the exposure reference value we set above 
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 7B.g Attach the labels of the exposure contrasts
    est.table <- est.table %>% 
      full_join(expContrasts, by = 'counterfactual_temp')
    
    # 7B.h Save estimate table for individual lags 
    est.table %>%
      write.csv(here::here(outPath, 'estimates', 
                                       paste0('EstInd_', modelName, '.csv')))
    
    # 7B.i Extract cumulative coefficient fit and ci 
    # the cumulative coefficients represent the relative risk for a day 
    # if the previous set of days were at the 'exposed' temperature value 
    # rather than the reference temperature
    fit.table0 <- as.data.frame(est.ref.mean$cumRRfit) %>% 
      mutate(refT = 'mean') 
    
    fit.table <- fit.table0 %>% 
      bind_rows(as.data.frame(est.ref.per05$cumRRfit) %>% mutate(refT = 'per05'))
    
    colnames(fit.table) <- paste0('fit.rr.', colnames(fit.table))
    
    fit.table <- fit.table %>%  
      rename(refT = fit.rr.refT) %>% 
      mutate(counterfactual_temp = rep(as.numeric(row.names(fit.table0)), 2))
    
    lci.table <- as.data.frame(est.ref.mean$cumRRlow) %>% 
      bind_rows(as.data.frame(est.ref.per05$cumRRlow)) 
    colnames(lci.table) <- paste0('lci.rr.', colnames(lci.table))
    
    uci.table <- as.data.frame(est.ref.mean$cumRRhigh) %>% 
      bind_rows(as.data.frame(est.ref.per05$cumRRhigh) )
    colnames(uci.table) <- paste0('uci.rr.', colnames(uci.table))
    
    # 7B.j Combine fit and se for individual lags #vdo comment: instead "individual lags", this should really be "cumulative lags" yes? We already saved ind lags on line388-390
    # note that all RR are relative to the exposure reference value we set above 
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 7B.k Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'counterfactual_temp')
    
    # 7B.l Save cumulative estimates table 
    est.table %>% 
      write.csv(here::here(outPath, 'estimates',
                           paste0('EstCumul_', modelName, '.csv')))
  } 
  
}
